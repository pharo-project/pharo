lessons
loops
	^ Lesson
title: 'Loops' 
lesson: 
'"Loops are high-level collection iterators, implemented as regular methods."

"Basic loops:
  to:do:
  to:by:do"

1 to: 100 do:
  [:i | Transcript show: i asString; cr ].

1 to: 100 by: 3 do: [:i | Transcript show: i asString; cr].

100 to: 0 by: -2 do: 
    [:i | Transcript show: i asString; cr].

1 to: 1 do: [:i | ProfStef next].'