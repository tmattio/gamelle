include Gg
module P2 = P2_
module Segment = Segment
module Circle = Circle
module Box = Box
module Color = Color_

module Make (Draw : Draw.S) = struct
  module Shape = struct
    include Shape
    include Shape.Make (Draw)
  end

  module Physics = struct
    include Physics
    include Physics.Make (Draw)
  end
end
