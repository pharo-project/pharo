lessons
messageSyntaxExecutionOrder
	^ Lesson
title: 'Message syntax: Execution order' 
lesson: 
'"Unary messages are executed first, then binary messages and finally keyword messages:
    Unary > Binary > Keywords"

2 + 3 squared.

2 raisedTo: 3 + 2.
  
(0@0) class.

0@0 corner: 100@200.

(0@0 corner: 100@200) class.

"Between messages of similar precedence, expressions are executed from left to right"

-3 abs negated reciprocal.

ProfStef next.'