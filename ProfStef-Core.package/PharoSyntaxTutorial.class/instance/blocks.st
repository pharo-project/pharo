lessons
blocks
	^ Lesson
title: 'Blocks' 
lesson: 
'"Cascade is cool ! Let''s talk about blocks.

Blocks are anonymous methods that can be stored into variables and executed on demand.

Blocks are delimited by square brackets: []"

[Workspace open].

"does not open a Browser because the block is not executed.

Here is a block that adds 2 to its argument (its argument is named x):"

[:x | x+2].

"We can execute a block by sending it value messages."

[:x | x+2] value: 5.

[Workspace open] value.

[:x | x+2] value: 10.

[:x :y| x + y] value:3 value:5.

[ProfStef next] value.'