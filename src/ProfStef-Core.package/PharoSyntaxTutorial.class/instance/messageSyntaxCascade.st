lessons
messageSyntaxCascade
	^ Lesson
title: 'Message syntax: Cascade' 
lesson: 
'"; is the cascade operator. It''s useful to send message to the SAME receiver
Open a Transcript (console):"

Transcript open.

"Then:"

Transcript show: ''hello''.
Transcript show: ''Pharo''.
Transcript cr.

"is equivalent to:"

Transcript 
	   show: ''hello'';
	   show: ''Pharo'' ;
	   cr.

"Try to go to the next lesson with a cascade of two ''next'' messages:"

ProfStef'.