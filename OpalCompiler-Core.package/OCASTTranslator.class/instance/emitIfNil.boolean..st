inline messages factored
emitIfNil: aMessageNode boolean: aBoolean
	| args notNilBlock |
	
	valueTranslator visitNode: aMessageNode receiver.
	
	"emits the == nil code and push correct value on stack if the ifNotNil block has an argument"
	args := aMessageNode arguments.
	notNilBlock := aBoolean ifTrue: [args first] ifFalse: [args last].
	notNilBlock arguments ifNotEmpty: [ notNilBlock arguments first binding emitStore: methodBuilder ].
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.
	
	self emitCondition: args boolean: aBoolean