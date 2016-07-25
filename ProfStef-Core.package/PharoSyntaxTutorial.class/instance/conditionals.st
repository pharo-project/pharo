lessons
conditionals
	^ Lesson
title: 'Conditionals' 
lesson: 
'"Conditionals are just messages sent to Boolean objects"

1 < 2
  ifTrue: [100]
  ifFalse: [42].

"Here the message is ifTrue:ifFalse

Try this:"

Transcript open.

3 > 10 
	ifTrue: [Transcript show: ''maybe there''''s a bug ....'']
	ifFalse: [Transcript show: ''No : 3 is less than 10''].

3 = 3 ifTrue: [ProfStef next].'.