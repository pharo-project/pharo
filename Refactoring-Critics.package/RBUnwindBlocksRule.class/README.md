Move assignment out of unwind blocks.

For example

[[[
[ statements. 
var := object ] ifCurtailed: block
]]]


[[[ 
var := [ statements. 
	    object] ifCurtailed: block
]]]