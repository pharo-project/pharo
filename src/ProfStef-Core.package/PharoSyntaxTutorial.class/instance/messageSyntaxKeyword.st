lessons
messageSyntaxKeyword
	^ Lesson
title: 'Message syntax: Keyword messages' 
lesson: 
'"Keyword Messages are messages with arguments. They have the following form:
    anObject akey: anotherObject akey2: anotherObject2"

4 between: 0 and: 10.

"The message is between:and: sent to the Number 4"

1 max: 3.

Color r:1 g:0 b:0.

"The message is r:g:b: implemented on class Color. Note you can also write"

Color
	r:1
	g:1
	b:0.
	
ProfStef perform: #next.'