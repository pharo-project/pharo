parsing
parseType
	" parse type name and optional number of asterisks, following it"
	| typeName ptrArity |

	typeName := self parseWord.
	typeName isNil ifTrue: [ ^ self error: 'type name expected' ].

	"skip 'const' , which is often used but has no any use for us "	
	typeName = 'const' ifTrue: [
		self skipSpace.
		typeName := self parseWord.
		typeName isNil ifTrue: [ ^ self error: 'type name expected' ] ].
	
	ptrArity := 0.
	[ self skipSpace. stream peek = $* ] 
	whileTrue: [ 
		ptrArity := ptrArity + 1.
		stream next ].

	"Answer a tuple name, arity"
	^ { typeName. ptrArity }