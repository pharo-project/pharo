initializing
initializeWithName: nameString
superclassName: superclassString
traitComposition: traitCompositionString
classTraitComposition: classTraitCompositionString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbol
comment: commentString
commentStamp: stampStringOrNil
	name := nameString asSymbol.
	superclassName := superclassString ifNil: ['nil'] ifNotNil: [superclassString asSymbol].
	traitComposition := traitCompositionString.
	classTraitComposition := classTraitCompositionString.
	category := categoryString.
	name = #CompiledMethod ifTrue: [type := #compiledMethod] ifFalse: [type := typeSymbol].
	comment := commentString withSqueakLineEndings.
	commentStamp := stampStringOrNil ifNil: [self defaultCommentStamp].
	variables := OrderedCollection  new.
	self addVariables: ivarArray ofType: MCInstanceVariableDefinition.
	self addVariables: cvarArray asSortedCollection ofType: MCClassVariableDefinition.
	self addVariables: poolArray asSortedCollection ofType: MCPoolImportDefinition.
	self addVariables: civarArray ofType: MCClassInstanceVariableDefinition.