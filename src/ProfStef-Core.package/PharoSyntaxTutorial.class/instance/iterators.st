lessons
iterators
	^ Lesson
title: 'Iterators' 
lesson: 
'"The message do: is sent to a collection of objects (Array, Set, OrderedCollection), evaluating the block for each element.

Here we want to print all the numbers on the Transcript (a console)"

#(11 38 3 -2 10) do: [:each |
     Transcript show: each printString; cr].

"Some other really nice iterators"

#(11 38 3 -2 10) collect: [:each | each abs].

#(11 38 3 -2 10) collect: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each > 10].

#(11 38 3 -2 10) reject: [:each | each > 10].

#(11 38 3 -2 10) 
     do: [:each | Transcript show: each printString]
     separatedBy: [Transcript show: ''.''].


ProfStef allInstances do: [:aPharoTutorial | aPharoTutorial next].'