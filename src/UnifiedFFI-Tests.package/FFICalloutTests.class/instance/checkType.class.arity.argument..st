private
checkType: type class: typeClass arity: ptrArity argument: arg 

	self assert: type class equals: typeClass.
	self assert: type pointerArity equals: ptrArity.
	self assert: type loader class equals: FFIMethodArgument.
	self assert: type loader argName equals: arg 