lessons
reflection
	^ Lesson
title: 'Reflection' 
lesson: 
'"You can inspect and change the system at runtime.

Take a look at method #ifFalse:ifTrue: source code of class True:"

(True>>#ifFalse:ifTrue:) definition.

"Or just its comment:"

(True>>#ifFalse:ifTrue:) comment.

"Here''s all the methods I implement:"

ProfStef selectors.


"Let''s create a new method to go to the next lesson:"

ProfStef class compile:''goToNextLesson
  self next''.

"Wow ! I can''t wait to use my new method ! "

ProfStef goToNextLesson.'