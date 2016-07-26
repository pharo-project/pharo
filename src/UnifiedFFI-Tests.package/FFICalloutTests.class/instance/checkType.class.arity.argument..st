private
checkType: type class: typeClass arity: ptrArity argument: arg 

	self assert: type class = typeClass.
	self assert: type pointerArity = ptrArity.
	self assert: type loader class = FFIMethodArgument.
	self assert: type loader argName = arg 