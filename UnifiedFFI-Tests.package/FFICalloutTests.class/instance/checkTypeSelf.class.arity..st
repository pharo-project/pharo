private
checkTypeSelf: type class: typeClass arity: ptrArity 

	self assert: type class = typeClass.
	self assert: type pointerArity = ptrArity.
	self assert: type loader class = FFISelfArgument.