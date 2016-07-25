Checks for methods whose source sends a different super message.

A common example of this is in creation methods. You might define a method such as:

	createInstance
		^super new initialize

If the new method is not defined in the class, you should probably rewrite this to use self instead. Also, if the new method is defined, you might question why you need to send new to the superclass instead of to the class.