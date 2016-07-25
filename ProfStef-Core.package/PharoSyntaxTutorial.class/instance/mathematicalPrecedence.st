lessons
mathematicalPrecedence
	^ Lesson
title: 'Mathematical precedence'
lesson: 
'"Traditional precedence rules from mathematics do not apply in Pharo."

2 * 10 + 2.

"Here the message * is sent to 2, which answers 20, then 20 receive the message +

Remember that all messages always follow a simple left-to-right precedence rule, * without exceptions *."

2 + 2 * 10.

2 + (2 * 10).

8 - 5 / 2.

(8 - 5) / 2.

8 - (5 / 2).

ProfStef next.'