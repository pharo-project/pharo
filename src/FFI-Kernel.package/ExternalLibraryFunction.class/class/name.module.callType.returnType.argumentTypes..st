instance creation
name: aName module: aModule callType: callType returnType: retType argumentTypes: argTypes
	^self new
		name: aName
		module: aModule
		flags: callType
		argTypes: (Array with: retType), argTypes