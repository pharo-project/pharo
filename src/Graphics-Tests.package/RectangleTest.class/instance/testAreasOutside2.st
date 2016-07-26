tests
testAreasOutside2
    | frame rects visibleArea  |
    frame := 0@0 extent: 300@300.
    rects := OrderedCollection new: 80.
    rects add: (50@50 corner: 200 @ 200);
          add: (100@100 corner: 250@250).
  
   visibleArea := Array with: frame.
   rects do: [:rect |  | remnants |
      remnants := OrderedCollection new.
      visibleArea do: [:a | remnants addAll: (a areasOutside: rect)].
      visibleArea := remnants.
  ].
  visibleArea := visibleArea asArray.
  self assert: (visibleArea allSatisfy: [:r | r area ~= 0]).

   1 to: visibleArea size do: [:idx |
     idx + 1 to: visibleArea size do: [:idx2 |
        self deny: ((visibleArea at: idx) intersects: (visibleArea at: idx2)).
  ]  ].

  1 to: rects size do: [:idx |
     1 to: visibleArea size do: [:idx2 |
        self deny: ((rects at: idx) intersects: (visibleArea at: idx2)).
  ]  ].

