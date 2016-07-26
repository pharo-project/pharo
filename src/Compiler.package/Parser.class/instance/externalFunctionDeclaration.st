primitives
externalFunctionDeclaration
	"Parse the function declaration for a call to an external library."

	| descriptorClass callType retType externalName args argType module |
	descriptorClass := Smalltalk globals at: #ExternalFunction ifAbsent: [ nil ].
	descriptorClass == nil 
		ifTrue: [ ^ false ].
	callType := descriptorClass callingConventionFor: here.
	callType == nil 
		ifTrue: [ ^ false ].	"Parse return type"
	self advance.
	retType := self externalType: descriptorClass.
	retType == nil 
		ifTrue: [ ^ self expected: 'return type' ].	"Parse function name or index"
	externalName := here.
	(self match: #string)
		ifTrue: [ externalName := externalName asSymbol ]
		ifFalse: [ (self match: #number)
						ifFalse: [ ^ self expected: 'function name or index' ] ].
	(self matchToken: #'(')
		ifFalse: [ ^ self expected: 'argument list' ].
	args := Array new writeStream.
	[ here == #')' ]
		whileFalse: [ 
			argType := self externalType: descriptorClass.
			argType == nil
				ifTrue: [ ^ self expected: 'argument' ].
			argType isVoid & argType isPointerType not
				ifFalse: [ args nextPut: argType ] ].
	(args position = self properties selector numArgs) ifFalse: [
		^self expected: 'Matching number of arguments'].
	(self matchToken: #')')
		ifFalse: [ ^ self expected: ')' ].
	(self matchToken: 'module:')
		ifTrue: [ 
			module := here.
			(self match: #string)
				ifFalse: [ ^ self expected: 'String' ].
			module := module asSymbol ].
	Smalltalk globals
		at: #ExternalLibraryFunction
		ifPresent: [ :xfn | 
			| fn |
			fn := xfn
				name: externalName
				module: module
				callType: callType
				returnType: retType
				argumentTypes: args contents.
			self allocateLiteral: fn ].
	self addPragma: (Pragma keyword: #primitive: arguments: #(120)).
	^ true