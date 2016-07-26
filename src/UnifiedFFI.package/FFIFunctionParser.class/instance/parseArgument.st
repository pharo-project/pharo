parsing
parseArgument
	| argument argName indirectIndex typeAndPtrArity char |
	" An argument can be:

 - nil , true , false
 - a class/pool variable name
 - integer literal

 - self , with optional **
 - type name *** arg name

"
	char := stream peek.
	(char isDigit or: [ char = $- ])
		ifTrue: [ ^ self requestor integerConstantArgument: self parseInt ].
	typeAndPtrArity := self parseType.
	self skipSpace.
	argName := self parseWord.
	self skipSpace.
	char := stream peek.
	char = $@
		ifTrue: [ 
			stream next.
			self skipSpace.
			argName := argName.
			indirectIndex := self parseInt ].	
	"for sole 'void' fake argument "
	(typeAndPtrArity = #('void' 0) and: [ argName isNil ])
		ifTrue: [ ^ nil ].
	argument := requestor
		argName: argName
		indirectIndex: indirectIndex
		type: typeAndPtrArity first
		ptrArity: typeAndPtrArity second.
	argument isExternalType
		ifTrue: [ argument validateAfterParse: typeAndPtrArity ].
	^ argument 