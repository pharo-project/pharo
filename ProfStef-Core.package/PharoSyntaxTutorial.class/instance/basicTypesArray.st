lessons
basicTypesArray
	^ Lesson
title: 'Basic types: Array' 
lesson: 
'"Literal arrays are created at parse time:"

#(1 2 3).

#( 1 2 3 #(4 5 6)) size.

#(1 2 4) isEmpty.

#(1 2 3) first.

#(''hello'' ''World'') at: 2 put: ''Pharo''; yourself.

ProfStef next.'