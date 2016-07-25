tests
testAreasOutside1

    | frame rects visibleArea  |
    frame := 0@0 extent: 300@300.
    rects := OrderedCollection new: 80.
    0 to: 3 do: [:i |
      0 to: 2 do: [:j |
            rects add: (i@j * 20 extent: 10@10)
    ]  ].
  
   visibleArea := Array with: frame.
   rects do: [:aRectangle |  | remnants |
      remnants := OrderedCollection new.
      visibleArea do: [:a | remnants addAll: (a areasOutside: aRectangle)].
      visibleArea := remnants.
  ].
  visibleArea := visibleArea asArray.
  self assert: (visibleArea allSatisfy: [:r | r area ~= 0]).
   1 to: visibleArea size do: [:index |
     index + 1 to: visibleArea size do: [:index2 |
        self deny: ((visibleArea at: index) intersects: (visibleArea at: index2)).
  ]  ].

  1 to: rects size do: [:index |
     1 to: visibleArea size do: [:index2 |
        self deny: ((rects at: index) intersects: (visibleArea at: index2)).
  ]  ]