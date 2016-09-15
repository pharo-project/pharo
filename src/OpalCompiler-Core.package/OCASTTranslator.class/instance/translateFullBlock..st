visitor-double dispatching
translateFullBlock: aBlockNode
	| argumentNames |
	methodBuilder compilationContext: aBlockNode methodNode compilationContext.
	
	"args, then copied, then temps"
	methodBuilder addTemps: (argumentNames := aBlockNode argumentNames asArray).
	methodBuilder addTemps: (aBlockNode scope inComingCopiedVars asArray collect: [:each | each name]).
	methodBuilder addTemps: ((aBlockNode scope tempVars asArray collect: [ :each | each name]) copyWithoutAll: argumentNames).
	
	methodBuilder numArgs: argumentNames size.
	
	aBlockNode scope tempVector ifNotEmpty: [
		methodBuilder 
			createTempVectorNamed: aBlockNode scope tempVectorName 
			withVars: (aBlockNode scope tempVector collect: [:each| each name]) asArray.
	].
	valueTranslator visitNode: aBlockNode body.
	methodBuilder addBlockReturnTopIfRequired.
	aBlockNode ir: self ir.
	^ aBlockNode ir compiledBlock: aBlockNode scope