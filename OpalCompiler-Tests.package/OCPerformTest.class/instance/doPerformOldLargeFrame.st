helper methods
doPerformOldLargeFrame
	| t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 |
	"The perform primitive reuses the current stack frame.
	When this was big,the following perform would succeed, since the stack frame was large enough to fit all the arguments"
	
	t1 := t2 := t3 := t4 := t5 := t6 := t7 := t8 := t9 := t10 := t11 := t12 := t13 := 1.
	
	self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:
		withArguments: (Array new: 15).

	^ t1