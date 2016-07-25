lessons
blocksAssignation
	^ Lesson
title: 'Block assignation' 
lesson: 
'"Blocks can be assigned to a variable then executed later.

Note that |b| is the declaration of a variable named ''b'' and that '':='' assigns a value to a variable.

Select the three lines then Print It:"

|b|
b := [:x | x+2].
b value: 12.


ProfStef next.'