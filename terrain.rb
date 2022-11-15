require 'rubygems'

require 'sinatra'

get '/' do
  erb :index
end


class TopoGrid

  attr :cx_list

  def initialize
    @cells = []
    @cx_list = []
  end


  def set_cell(j,k,key,value)
    cell = get_cell(j,k)
    if cell == nil
      cell = {s: j, t: k}
      @cells << cell
    end
    cell[key] = value if cell.class == Hash
  end


  def get_cell(j,k)
    @cells.find {|cc| cc[:s] == j && cc[:t] == k}
  end


  def get_all_cells
    @cells
  end


end



class SquareTopoGrid < TopoGrid

  attr :m, :n

  def initialize(m,n)
    super()
    @m = m
    @n = n

    1.upto(@m) do |j|
      1.upto(@n) do |k|
        zz = {s: j, t: k, N: :blank, E: :blank, S: :blank, W: :blank}
        @cells << zz
      end
    end

    # block edges
    @cells.each do |cc|
      cc[:N] = :blocked if cc[:t] == 1
      cc[:W] = :blocked if cc[:s] == 1
      cc[:E] = :blocked if cc[:s] == @m
      cc[:S] = :blocked if cc[:t] == @n
    end

  end


end



class RiverTopoGrid < SquareTopoGrid

  def initialize
    super(5,3)

    set_river_mouth

    # build first segment with "mouth" cell as root
    build_river_mouth_topo_segment

    build_available_topo_connection_list

    # iterate randomly through connections until empty
    until @open_list == []
      @open_list.shuffle!
      connection = @open_list.pop
      s1 = connection[:s1]
      t1 = connection[:t1]
      dir1 = connection[:dir1]
      seg1 = get_cell(s1,t1)[:seg]
      s2 = connection[:s2]
      t2 = connection[:t2]
      dir2 = connection[:dir2]
      seg2 = get_cell(s2,t2)[:seg]

      # determine if cells in new connection
      # are already part of segments
      if seg1 == nil
        status = seg2 == nil ? :new_seg : :add_c1_to_seg2
      else
        status = seg2 == nil ? :add_c2_to_seg1 : :join_segs
        status = :no_connect if seg1 == seg2
      end

      case status

      # join segments, create new segment, or add new connection to an
      # existing segment as appropriate
      when :new_seg
        seg = new_topo_segment(s1,t1,s2,t2)
        @segments << seg
      when :add_c1_to_seg2
        add_cell_to_topo_segment(s1,t1,seg2)
      when :add_c2_to_seg1
        add_cell_to_topo_segment(s2,t2,seg1)
      when :join_segs
        join_topo_segments(seg1,seg2)
      end

      if status != :no_connect
        set_cell(s1,t1,dir1,:connect)
        set_cell(s2,t2,dir2,:connect)
        add_topo_connection(s1,t1,s2,t2,dir1,dir2)
      end
    end

    add_topo_connection(@mouth,@n,@mouth,@n+1,:S,:N)
  end


  def set_river_mouth
    @mouth = 2+rand(@m-2)
    set_cell(@mouth,@n,:S,:connect)
    set_cell(@mouth,@n+1,:s,@mouth)
    set_cell(@mouth,@n+1,:t,@n+1)
    set_cell(@mouth,@n+1,:N,:connect)
    set_cell(@mouth,@n+1,:is_mouth,true)
  end


  # build first segment with "mouth" cell as root
  def build_river_mouth_topo_segment
    @root = 0
    set_cell(@mouth,@n,:seg,@root)
    @root_s = @mouth
    @root_t = @n
    root_node = {s: @root_s, t: @root_t}
    @tree = [root_node]
    @segments = []
    @segments[@root] = @tree
  end


  def build_available_topo_connection_list
    @open_list = []
    1.upto(@m-1) do |s|
      1.upto(@n) do |t|
        ct = {s1: s, t1: t, dir1: :E, s2: s+1, t2: t, dir2: :W}
        @open_list << ct
      end
    end
    1.upto(@m) do |s|
      1.upto(@n-1) do |t|
        ct = {s1: s, t1: t, dir1: :S, s2: s, t2: t+1, dir2: :N}
        @open_list << ct
      end
    end
  end


  def new_topo_segment(s1,t1,s2,t2)
    seg_ctr = @segments.length
    set_cell(s1,t1,:seg,seg_ctr)
    set_cell(s2,t2,:seg,seg_ctr)
    seg = [ {s: s1, t: t1},{s: s2, t: t2} ]
    seg
  end


  def add_cell_to_topo_segment(s,t,seg)
    set_cell(s,t,:seg,seg)
    br = @segments[seg]
    cc = {s: s, t: t}
    br << cc
    @segments[seg] = br
  end


  def join_topo_segments(seg1,seg2)
    seg = [seg1,seg2].min
    segx = [seg1,seg2].max
    brx = @segments[segx]
    brx.each do |cc|
      set_cell(cc[:s],cc[:t],:seg,seg)
      cc[:seg] = seg
    end
    combo = @segments[seg1]+@segments[seg2]
    @segments[seg] = combo
    @segments[segx] = []
  end


  def add_topo_connection(s1,t1,s2,t2,dir1,dir2)
    cx = {s1: s1, t1: t1, dir1: dir1, s2: s2, t2: t2, dir2: dir2}
    @cx_list << cx
  end


  # make a string to pass to the DOM so a coffeescript method can draw a
  # diagram of the topo grid
  def to_dom_string
    str = ""
    1.upto(@n) do |k|
      1.upto(@m) do |j|
        cell = get_cell(j,k)
        [:N,:E,:S,:W].each do |dir|
          sym = cell[dir] if cell.respond_to? :[]
          ch = sym == :connect ? "c" : "x"
          str << ch
        end
      end
    end
    str
  end


end



class HexMap

  # Map Constants

  # Current map dimensions are 41 hexes East to West [0..40] and 23 hexes
  # North to South [0.22].
  HEX_DIM_EW = 40
  HEX_DIM_NS = 22

  OPP_DIRS = { N: :S,
               NE: :SW,
               SE: :NW,
               S: :N,
               SW: :NE,
               NW: :SE }

  LAND_SYMBOLS = [
    :elev_010,
    :elev_020,
    :elev_030,
    :elev_040,
    :elev_050,
    :elev_060,
    :elev_070,
    :elev_080,
    :elev_090,
    :elev_100,
    :elev_110,
    :elev_120,
    :elev_130,
    :elev_140 ]

  WATER_SYMBOLS = [
    :water,
    :water_010,
    :water_020,
    :water_030,
    :water_040,
    :water_050,
    :water_060,
    :water_070,
    :water_080,
    :water_090,
    :water_100,
    :water_110,
    :water_120,
    :water_130,
    :water_140,
    :zone ]


  def initialize
    @grid = []
    fill(:no_data)
  end


# TODO Why isn't this giving us the 'repeat array' problem? We should change it anyway.
  def fill(value)
    0.upto(HEX_DIM_EW) {|i| @grid[i] = [value]*(HEX_DIM_NS+1)}
  end


  def mark_hexes(value,hexes)
    hexes.each {|hx| put(hx,value)} unless hexes == nil
  end


  def put(hex,value)
    z = :good
    if hex == nil
      z = :bad
    else
      a = hex[:a]
      b = hex[:b]
      z = :bad if (a == nil || a<0 || a>HEX_DIM_EW || b == nil || b<0 || b>HEX_DIM_NS)
    end
    put_ab(a,b,value) if z == :good
  end


  def put_ab(a,b,value)
    @grid[a][b] = value
  end


  def get(hex)
    z = :good
    if hex == nil
      z = :bad
    else
      a = hex[:a]
      b = hex[:b]
      z = :bad if a == nil || a<0 || a>HEX_DIM_EW || b == nil || b<0 || b>HEX_DIM_NS
    end
    get_ab(a,b) if z == :good
  end


  def get_ab(a,b)
    @grid[a][b]
  end


  def hex_find_all(value)
    hexes = []
    @grid.each_index do |a|
      @grid[a].each_index do |b|
        hexes << {a: a, b: b} if @grid[a][b] == value
      end
    end
    hexes
  end


  # returns all hexes adjacent to a set of hexes that are not part of that set
  def all_adjacent(hexes)
    nn = []
    hexes.each do |hx|
      [:N,:NE,:SE,:S,:SW,:NW].each do |dir|
        hxx = next_hex(hx,dir)
        nn << hxx unless nn.include?(hxx)
      end
    end
    adj = nn - hexes
    adj
  end


  # returns all hexes adjacent to a single hex
  def adj_hexes(hex)
    adj = []
    [:N,:NE,:SE,:S,:SW,:NW].each do |dir|
      hxx = next_hex(hex,dir)
      adj << hxx unless hxx == nil
    end
    adj
  end


  # merge data from another HexMap object with this one
  def merge_data(hex_grid2,mode)
    case mode
    when :rivers
      symbols = WATER_SYMBOLS
    end
    0.upto(HEX_DIM_EW) do |a|
      0.upto(HEX_DIM_NS) do |b|
        value = hex_grid2.get_ab(a,b)
        put_ab(a,b,value) if symbols.include?(value)
      end
    end
  end


  # returns true if ANY path exists in zone[] connecting hex1 and hex2
  def path_exists?(zone,hex1,hex2)
    root = hex1.clone
    branches = [root]
    zone << hex2
    path_found = false

    until path_found || branches.empty?
      branches.shuffle!
      ptr = branches.pop
      nbrs = zone.find_all {|z| adjacent?(z,ptr)}
      if nbrs.include?(hex2)
        path_found = true
      else
        branches = branches + nbrs
        zone = zone - nbrs
      end
    end
    path_found
  end


  # returns true if hex1 is adjacent to hex2
  def adjacent?(hex1,hex2)
    a1 = hex1[:a]
    b1 = hex1[:b]
    a2 = hex2[:a]
    b2 = hex2[:b]
    adj = false
    adj = true if a1 == a2 && (b2 == b1-1 || b2 == b1+1)
    adj = true if b1 == b2 && (a2 == a1-1 || a2 == a1+1)
    adj = true if a1%2 == 0 && (a2 == a1-1 || a2 == a1+1) && b2 == b1+1
    adj = true if a1%2 == 1 && (a2 == a1-1 || a2 == a1+1) && b2 == b1-1
    adj
  end


  # Make a vector of n hexes in a row in direction dir
  def make_vector(hex,dir,n)
    vector = [hex]
    hh = hex
    1.upto(n-1) do
      hh = next_hex(hh,dir)
      vector << hh
    end
    vector
  end


  def next_hex(hex,dir)
    nxt = {}
    if hex == nil
      nxt = nil
    else
      a = hex[:a]
      b = hex[:b]
      nxt = nil if a == nil || b == nil
    end
    if nxt != nil
      case dir
      when :no_go
        nxt[:a] = a
        nxt[:b] = b
      when :N
        nxt[:a] = a
        nxt[:b] = b-1
      when :NE
        nxt[:a] = a+1
        nxt[:b] = b-a%2
      when :SE
        nxt[:a] = a+1
        nxt[:b] = b-a%2+1
      when :S
        nxt[:a] = a
        nxt[:b] = b+1
      when :SW
        nxt[:a] = a-1
        nxt[:b] = b-a%2+1
      when :NW
        nxt[:a] = a-1
        nxt[:b] = b-a%2
      else
        nxt = nil
      end
      nxt = nil if nxt[:a]<0 || nxt[:a]>HEX_DIM_EW || nxt[:b]<0 || nxt[:b]>HEX_DIM_NS
    end
    nxt
  end


  def opp_dir(dir)
    OPP_DIRS[dir]
  end


end



class TerrainMap < HexMap

  attr :rivers

  def initialize
  super
  @rivers = RiverMap.new
  merge_data(@rivers,:rivers)
  build_terrain_data
  end


  def build_terrain_data
    @fill_zone = FillBoundary.new(self)
    @fill_zone.fill until @fill_zone.finished
  end


  def to_dom_string
    str = ""
    @grid.each do |t|
      t.each {|hex| str << encode(hex)}
    end
    str
  end


  SHOW_WATER = :solid

  # encode elevation values to a one-character code
  def encode(elev)

    values = {
      elev_010: "a",
      elev_020: "b",
      elev_030: "c",
      elev_040: "d",
      elev_050: "e",
      elev_060: "f",
      elev_070: "g",
      elev_080: "h",
      elev_090: "i",
      elev_100: "j",
      elev_110: "k",
      elev_120: "l",
      elev_130: "m",
      elev_140: "n",
      water_010: "A",
      water_020: "B",
      water_030: "C",
      water_040: "D",
      water_050: "E",
      water_060: "F",
      water_070: "G",
      water_080: "H",
      water_090: "I",
      water_100: "J",
      water_110: "K",
      water_120: "L",
      water_130: "M",
      water_140: "N",
      water: "A",
      zone: "Z" }
    ch = values.fetch(elev,:no_data)
    ch = "x" if ch == :no_data
    ch = "A" if ch.between?("A","N") && SHOW_WATER == :solid
    ch
  end


end



# A HexMap subclass with only the river hexes filled, to be later superimposed
# upon the main terrain map.

class RiverMap < HexMap

  RIVER_START_EDGE = :S

  attr :river_mouth_hex, :river_topo, :river_hexes

  def initialize
    super
    fill(:no_data)

    # generate a model for the topology of river branching
    @river_topo = RiverTopoGrid.new

    # extract the list of cell to cell connections from the topology model
    @cx_list = @river_topo.cx_list

    # add river branching points corresponding to the topology designated
    # in the 'river topo grid'
    @river_topo.get_all_cells.each {|cell| add_connector(cell)}

    # connect the branching points to each other
    @cx_list.each {|cx| connect_cx(cx)}

    @river_hexes = hex_find_all(:water)

    @river = RiverSystem.new(self)


# FIXME Probably put the RiverSystem.new statement inside the generate_water_elevation method:

    # generate water elevation
    generate_water_elevation

  end


  # place a river connector (branch point) on the terrain map in the zone
  # corresponding to a cell in the river topo grid
  def add_connector(topo_cell)
    connector = HexConnector.new(topo_cell,self)
    @river_mouth_hex = connector.river_mouth_hex if connector.river_mouth_hex != nil
    mark_hexes(:water, connector.get_hexes)
    connector.get_connection_points.each_pair do |dir,hex|
      s = topo_cell[:s]
      t = topo_cell[:t]
      c1 = @cx_list.find {|cc| cc[:s1] == s && cc[:t1] == t && cc[:dir1] == dir}
      c2 = @cx_list.find {|cc| cc[:s2] == s && cc[:t2] == t && cc[:dir2] == dir}
      c1[:hex1] = hex unless c1 == nil
      c2[:hex2] = hex unless c2 == nil
    end
  end


  def get_start_point(cell,pattern)
    s = cell[:s]
    t = cell[:t]

    # (the '+3' and '+2' are to temporarily set all connectors to a set spot
    #   in the zone, later we'll set them at random locations within the zone)
    aa = (s-1)*RT_ZONE_WIDTH+3
    bb = (t-1)*RT_ZONE_HEIGHT+2

    # for the river mouth, use special values
    if pattern == :cx_river_mouth
      aa = aa+1
      bb = HEX_DIM_NS
    end

    {a: aa, b: bb}
  end


  # determine the connector pattern for a cell in the river topo grid
  def get_pattern(cell)
    cx = "xxxx"
    cx[0] = "N" if cell[:N] == :connect
    cx[1] = "E" if cell[:E] == :connect
    cx[2] = "S" if cell[:S] == :connect
    cx[3] = "W" if cell[:W] == :connect
    cx = "mouth" if cell[:is_mouth] == true
    pattern = select_random_match(cx.to_sym,CX_PATTERN_OPTIONS)
    pattern
  end


  # make the actual connection between two river connector branch points and
  # return the path of that connection
  def connect_cx(cx)

    # extract values from input parameter cx
    s1 = cx[:s1]
    t1 = cx[:t1]
    hex1 = cx[:hex1]
    a1= hex1[:a]
    b1 = hex1[:b]
    dir1 = cx[:dir1]
    s2 = cx[:s2]
    t2 = cx[:t2]
    hex2 = cx[:hex2]
    a2 = hex2[:a]
    b2 = hex2[:b]
    dir2 = cx[:dir2]
    case dir1
    when :S
      mode = :vert
      north_point = hex1
      south_point = hex2
    when :N
      mode = :vert
      north_point = hex2
      south_point = hex1
    when :E
      mode = :horz
      west_point = hex1
      east_point = hex2
    when :W
      mode = :horz
      west_point = hex2
      east_point = hex1
    end

    # get the zone of possible connector hexes for vertical or horizontal
    # connection
    case mode

    # get top and bottom bounding lanes for zone for a vertical connection
    when :vert
      max_west = 8*s1-7
      max_east = 8*s1-2

      top = []
      hx = north_point
      until hx[:a] == max_west || hx == south_point
        hx = next_hex(hx,:SW)
        top << hx
      end
      top << next_hex(north_point,:S)
      hx = north_point
      until hx[:a] == max_east
        hx = next_hex(hx,:SE)
        top << hx
      end
      top.sort_by! {|hh| hh[:a]}

      btm = []
      hx = south_point
      until hx[:a] == max_west
        hx = next_hex(hx,:NW)
        btm << hx
      end
      btm << next_hex(south_point,:N)
      hx = south_point
      until hx[:a] == max_east
        hx = next_hex(hx,:NE)
        btm << hx
      end
      btm.sort_by! {|hh| hh[:a]}

      hhxx1 = north_point
      hhxx2 = south_point

    # get top and bottom bounding lanes for zone for a horizontal connection
    when :horz
      a1 = west_point[:a]
      b1 = west_point[:b]
      a2 = east_point[:a]
      b2 = east_point[:b]
      delta_a = a2-a1
      delta_b = b2-b1
      zpat = ((delta_a-a1%2+1)/2).to_i
      delta_a_top = zpat-delta_b
      a_top = a1+delta_a_top
      zpab = -((delta_a+a1%2)/2).to_i
      delta_a_btm = delta_b-zpab
      a_btm = a1+delta_a_btm
      zpbt = ((delta_a+a1%2-1)/2).to_i
      delta_b_top = ((delta_b-zpbt)/2).to_i
      b_top = b1+delta_b_top
      zpbb = -((delta_a-a1%2)/2).to_i
      delta_b_btm = ((delta_b-zpbb+1)/2).to_i
      b_btm = b1+delta_b_btm

      top = []
      max_west = west_point[:a]+1
      hx = {a: a_top, b: b_top}
      top << hx

      until hx[:a] == max_west
        hx = next_hex(hx,:SW)
        top << hx
      end

      hx = {a: a_top, b: b_top}
      max_east = east_point[:a]-1
      until hx[:a] == max_east
        hx = next_hex(hx,:SE)
        top << hx
      end
      top.sort_by! {|hh| hh[:a]}

      btm = []
      hx = {a: a_btm, b: b_btm}
      btm << hx
      until hx[:a] == max_west
        hx = next_hex(hx,:NW)
        btm << hx
      end

      hx = {a: a_btm, b: b_btm}
      until hx[:a] == max_east
        hx = next_hex(hx,:NE)
        btm << hx
      end
      btm.sort_by! {|hh| hh[:a]}

      hhxx1 = west_point
      hhxx2 = east_point

    end

    # use the top and bottom bounding lanes to construct the connection zone
    zone = []
    zone_width = 0
    zone_width = top.length if top.respond_to?(:length)
    0.upto(zone_width-1) do |i|
      top_hex = top[i]
      btm_hex = btm[i]
      aa = top_hex[:a]
      b1 = top_hex[:b]
      b2 = btm_hex[:b]
      b1.upto(b2) {|bb| zone << {a: aa, b: bb} }
    end

    # pick a random connecting path through the zone
    path = []
    until zone == []
      zone.shuffle!
      hx = zone.pop
      path << hx unless path_exists?(zone+path,hhxx1,hhxx2)
    end
    path.each {|hx| put(hx,:water)}

  end


  def generate_water_elevation

  end


end



# A HexConnector object is a set of one or more adjacent hexes that serve as
# a junction point to connect two or more segments in a hex grid map, or to
# terminate a single segment. A HexConnector object corresponds to a cell in
# the topo grid.

class HexConnector

  # River Branch Connector Templates:
  # These templates give hex patterns for different kinds of river junctions.
  # Within an individual template array, there is one hash for each hex in that
  # pattern. :src is the index number in the array to move from to get the next
  # hex in the pattern. A value of :origin for the key :src means move from the
  # starting hex for that pattern instead. The "starting hex" may be empty in
  # some patterns. A value of :no_go for the key :dir means start at the origin
  # hex. The :cx_dir key has a value of :N, :S, :E or :W if it is the north, south,
  # east or west connector hex for that pattern.
  CX_PATTERN_OPTIONS = { mouth: :cx_river_mouth,
                         Nxxx: :cx_stub_n,
                         xExx: :cx_stub_e,
                         xxSx: :cx_stub_s,
                         xxxW: :cx_stub_w,
                         NESW: [:cx_4_a, :cx_4_b],
                         NESx: [:cx_rt_a, :cx_rt_b],
                         NxSW: [:cx_lft_a, :cx_lft_b],
                         NxSx: :cx_vert,
                         xExW: [:cx_hrz_a, :cx_hrz_b],
                         xESW: :cx_dn,
                         NExW: :cx_up,
                         NExx: :cx_n_e,
                         xESx: :cx_s_e,
                         xxSW: :cx_s_w,
                         NxxW: :cx_n_w }

  CX_TEMPLATES = {
    :cx_stub_n => [{src: :origin, dir: :no_go, cx_dir: :N}],
    :cx_stub_e => [{src: :origin, dir: :no_go, cx_dir: :E}],
    :cx_stub_s => [{src: :origin, dir: :no_go, cx_dir: :S}],
    :cx_stub_w => [{src: :origin, dir: :no_go, cx_dir: :W}],

    :cx_vert => [{src: :origin, dir: :no_go, cx_dir: :N},
                 {src: 0, dir: :S, cx_dir: :S}],

    :cx_hrz_a => [{src: :origin, dir: :S, cx_dir: :W},
                  {src: 0, dir: :NE, cx_dir: :E}],

    :cx_hrz_b => [{src: :origin, dir: :no_go, cx_dir: :W},
                  {src: 0, dir: :SE, cx_dir: :E}],

    :cx_n_e => [{src: :origin, dir: :no_go, cx_dir: :N},
                {src: 0, dir: :S, cx_dir: nil},
                {src: 1, dir: :SE, cx_dir: :E}],

    :cx_s_e => [{src: :origin, dir: :SE, cx_dir: :E},
                {src: 0, dir: :SW, cx_dir: nil},
                {src: 1, dir: :S, cx_dir: :S}],

    :cx_s_w => [{src: :origin, dir: :no_go, cx_dir: :W},
                {src: 0, dir: :SE, cx_dir: nil},
                {src: 1, dir: :S, cx_dir: :S}],

    :cx_n_w => [{src: :origin, dir: :SE, cx_dir: :N},
                {src: 0, dir: :S, cx_dir: nil},
                {src: 1, dir: :SW, cx_dir: :W}],

    :cx_dn => [{src: :origin, dir: :no_go, cx_dir: :W},
               {src: 0, dir: :SE, cx_dir: nil},
               {src: 1, dir: :S, cx_dir: :S},
               {src: 1, dir: :NE, cx_dir: :E}],

    :cx_up => [{src: :origin, dir: :SE, cx_dir: :N},
               {src: 0, dir: :S, cx_dir: nil},
               {src: 1, dir: :SW, cx_dir: :W},
               {src: 1, dir: :SE, cx_dir: :E}],

    :cx_rt_a => [{src: :origin, dir: :no_go, cx_dir: :N},
                 {src: 0, dir: :S, cx_dir: nil},
                 {src: 1, dir: :SE, cx_dir: nil},
                 {src: 2, dir: :S, cx_dir: :S},
                 {src: 2, dir: :NE, cx_dir: :E}],

    :cx_rt_b => [{src: :origin, dir: :SE, cx_dir: :N},
                 {src: 0, dir: :S, cx_dir: nil},
                 {src: 1, dir: :SE, cx_dir: :E},
                 {src: 1, dir: :SW, cx_dir: nil},
                 {src: 3, dir: :S, cx_dir: :S}],

    :cx_lft_a => [{src: :origin, dir: :S, cx_dir: :W},
                  {src: 0, dir: :SE, cx_dir: nil},
                  {src: 1, dir: :S, cx_dir: :S},
                  {src: 1, dir: :NE, cx_dir: nil},
                  {src: 3, dir: :N, cx_dir: :N}],

    :cx_lft_b => [{src: :origin, dir: :SE, cx_dir: :N},
                  {src: 0, dir: :S, cx_dir: nil},
                  {src: 1, dir: :SW, cx_dir: :W},
                  {src: 1, dir: :SE, cx_dir: nil},
                  {src: 3, dir: :S, cx_dir: :S}],

    :cx_4_a => [{src: :origin, dir: :SE, cx_dir: :N},
                {src: 0, dir: :S, cx_dir: nil},
                {src: 1, dir: :SW, cx_dir: :W},
                {src: 1, dir: :SE, cx_dir: nil},
                {src: 3, dir: :NE, cx_dir: :E},
                {src: 3, dir: :S, cx_dir: :S}],

    :cx_4_b => [{src: :origin, dir: :S, cx_dir: :W},
                {src: 0, dir: :SE, cx_dir: nil},
                {src: 1, dir: :S, cx_dir: :S},
                {src: 1, dir: :NE, cx_dir: nil},
                {src: 3, dir: :N, cx_dir: :N},
                {src: 3, dir: :SE, cx_dir: :E}],

    :cx_river_mouth => [{src: :origin, dir: :no_go, cx_dir: nil},
                        {src: 0, dir: :N, cx_dir: :N}] }

  RT_ZONE_WIDTH = 8
  RT_ZONE_HEIGHT = 8

  attr :topo_grid_cell, :hex_grid, :hexes, :connect_points, :river_mouth_hex

  def initialize(topo_cell,hex_grid)
    @topo_grid_cell = topo_cell
    @hex_grid = hex_grid
    @hexes = []
    @connect_points = {N: nil, E: nil, S: nil, W: nil}
    @river_mouth_hex = nil
    build_hexes
  end


  def get_hexes
    @hexes
  end


  def get_connection_points
    cx_points = @connect_points.select {|dir,hex| hex != nil}
    cx_points
  end


  def build_hexes

    pattern = get_pattern(@topo_grid_cell)
    start = get_start_point(@topo_grid_cell,pattern)
    prior_hexes = []

    # starting at the 'start point' hex, follow the pattern to build the connector
    template = CX_TEMPLATES[pattern]
    template.each do |step|
      src = step[:src]
      dir = step[:dir]
      h0 = src == :origin ? {a: start[:a], b: start[:b]} : prior_hexes[src]
      hex = @hex_grid.next_hex(h0,dir)
      prior_hexes << hex
      connect_points[step[:cx_dir]] = hex if [:N, :E, :S, :W].include?(step[:cx_dir])
      @hex_grid.put(hex,:water)
    end

    @river_mouth_hex = start if pattern == :cx_river_mouth

  end


  # determine the connector pattern for a cell in the river topo grid
  def get_pattern(cell)
    cx = "xxxx"
    cx[0] = "N" if cell[:N] == :connect
    cx[1] = "E" if cell[:E] == :connect
    cx[2] = "S" if cell[:S] == :connect
    cx[3] = "W" if cell[:W] == :connect
    cx = "mouth" if cell[:is_mouth] == true
    pattern = select_random_match(cx.to_sym,CX_PATTERN_OPTIONS)
    pattern
  end


  def get_start_point(cell,pattern)
    s = cell[:s]
    t = cell[:t]

    # (the '+3' and '+2' are to temporarily set all connectors to a set spot
    #   in the zone, later we'll set them at random locations within the zone)
    aa = (s-1)*RT_ZONE_WIDTH+3
    bb = (t-1)*RT_ZONE_HEIGHT+2

    # for the river mouth, use special values
    if pattern == :cx_river_mouth
      aa = aa+1
      bb = HexMap::HEX_DIM_NS
    end

    {a: aa, b: bb}
  end


end



# A class for data about the rivers

class RiverSystem

  WATER_ELEVATIONS = [
    :water_010,
    :water_020,
    :water_030,
    :water_040,
    :water_050,
    :water_060,
    :water_070,
    :water_080,
    :water_090,
    :water_100,
    :water_110,
    :water_120 ]


  def initialize(river_map)
    @river_hex_map = river_map
    @river_mouth = @river_hex_map.river_mouth_hex
    @endpoints = []
    @main_branch = get_branch_data(@river_mouth, RiverMap::RIVER_START_EDGE, :ocean)
    get_path_lengths
    set_water_elevations

  end


  def get_branch_data(root, back_direction, source)
    root[:down_stream] = {a: source[:a], b: source[:b]} if source != :ocean
    branches = [root]
    search_dir = [:N, :NE, :SE, :S, :SW, :NW] - [back_direction]
    search_hexes = {}
    search_dir.each {|dir| search_hexes[dir] = @river_hex_map.next_hex(root,dir)}
    branch_stubs = search_hexes.select {|dir,hex| @river_hex_map.get(hex) == :water}

    if branch_stubs == {}
      root[:is_endpoint] == true
      @endpoints << root
    else
      branch_stubs.each_pair {|dir,hex| branches << get_branch_data(hex, @river_hex_map.opp_dir(dir), root)}
      branches.flatten!
      root[:up_stream] = branch_stubs.map {|dir,hex| {a: hex[:a], b: hex[:b]} }
    end

    branches
  end


  def get_path_lengths
    max = 0
    @endpoints.each do |end_pt|
      count = 0
      hx = end_pt
        until hx == nil
          count += 1
          hx = get_downstream(hx)
        end
      end_pt[:path_length] = count
      max = [max,count].max
    end
    @max_path_length = max
  end


  def get_water_elev_vector
    ss = WATER_ELEVATIONS.size
    min_reps = @max_path_length/ss
    extras = @max_path_length%ss
    vector = WATER_ELEVATIONS.map {|el| [el]*min_reps}.flatten
    extras.times do
      rnd = rand(vector.size)
      vector.insert(rnd,vector[rnd])
    end
    vector
  end


  def set_water_elevations
    elev = get_water_elev_vector
    @river_hex_map.put(@river_mouth,WATER_ELEVATIONS[0])
    @endpoints.each do |end_pt|
      hx = end_pt
      p = end_pt[:path_length]-1
      until @river_hex_map.get(hx) != :water
        @river_hex_map.put(hx,elev[p])
        hx = get_downstream(hx)
        p -= 1
      end
    end
  end


  def get_downstream(hex)
    hxx = @main_branch.find {|hx| hx[:a] == hex[:a] && hx[:b] == hex[:b]}
    hxx[:down_stream]
  end


end



# This class represents the set of all hexes which are empty, but are adjacent
# to a hex that has data. When the fill method is finished, it returns the new
# fill boundary zone.

class FillBoundary


  NEXT_HIGHER = {
    :elev_010 => :elev_020,
    :elev_020 => :elev_030,
    :elev_030 => :elev_040,
    :elev_040 => :elev_050,
    :elev_050 => :elev_060,
    :elev_060 => :elev_070,
    :elev_070 => :elev_080,
    :elev_080 => :elev_090,
    :elev_090 => :elev_100,
    :elev_100 => :elev_110,
    :elev_110 => :elev_120,
    :elev_120 => :elev_130,
    :elev_130 => :elev_140,
    :elev_140 => :elev_140,
    :water_010 => :elev_020,
    :water_020 => :elev_030,
    :water_030 => :elev_040,
    :water_040 => :elev_050,
    :water_050 => :elev_060,
    :water_060 => :elev_070,
    :water_070 => :elev_080,
    :water_080 => :elev_090,
    :water_090 => :elev_100,
    :water_100 => :elev_110,
    :water_110 => :elev_120,
    :water_120 => :elev_130,
    :water_130 => :elev_140,
    :water_140 => :elev_140 }


  def initialize(map)
    @map = map
    @zone = build_init_zone
  end


  def build_init_zone
    hexes = @map.rivers.river_hexes
    zzn = get_edge_zone(hexes)
    zzn
  end


  def get_edge_zone(edge)
    search = @map.all_adjacent(edge)
    zzn = search.find_all {|hex| @map.get(hex) == :no_data}
    zzn
  end


  def fill
    hi_elev = @zone.map {|hex| higher_than_neighbors(hex)}
    @zone.each_index {|i| @map.put(@zone[i], hi_elev[i]) }
    @zone = get_edge_zone(@zone)
  end


  # returns the elevation value one level higher than the maximimum of the
  # elevations of neighboring hexes
  def higher_than_neighbors(hex)
    @map.adj_hexes(hex).map {|adj| NEXT_HIGHER[@map.get(adj)]}.compact.max
  end


  def finished
    fff = @zone == [] ? true : false
    fff
  end


end



# returns the value attached to the given key or, if key owns an array with
# multiple values, selects one of those values at random
def select_random_match(key,options_hash)
  mm = options_hash[key]
  match = nil if mm == nil
  match = mm if mm.class == Symbol
  match = mm.sample if mm.class == Array
  match
end


def terrain_string
  @map = TerrainMap.new
  @map.to_dom_string
end

def topo_string
  @r_topo = @map.rivers.river_topo
  @r_topo.to_dom_string
end

def get_m
  @r_topo.m
end

def get_n
  @r_topo.n
end
