visitor-double dispatching
visitBlockNode: aBlockNode
	| tempNames argumentNames copiedNames |
	aBlockNode arguments size >15 ifTrue: [self backendError: 'Too many arguments' forNode: aBlockNode ].
	
	aBlockNode isInlined ifTrue: [^ self visitInlinedBlockNode: aBlockNode ].
		
	argumentNames := aBlockNode argumentNames asArray.
	tempNames := (aBlockNode scope tempVars asArray collect: [ :each | each name]) copyWithoutAll: argumentNames.
	copiedNames := (aBlockNode scope  inComingCopiedVars asArray collect: [:each | each name]).
	
	aBlockNode hasMetalinkInstead ifFalse: [  
	methodBuilder
			pushClosureCopyCopiedValues: copiedNames
			args: argumentNames
			jumpTo:  #block.
	 
	aBlockNode scope tempVector ifNotEmpty: [
		methodBuilder 
			createTempVectorNamed: aBlockNode scope tempVectorName 
			withVars: (aBlockNode scope tempVector collect: [:each| each name]) asArray.
	].
	methodBuilder addTemps: tempNames.
	self emitMetaLinkBefore: aBlockNode.
	valueTranslator visitNode: aBlockNode body.
	methodBuilder addBlockReturnTopIfRequired.
	self flag: 'why dont we just add a blockReturnTop here... it could be removed or ignored in IRTranslator'.
	methodBuilder jumpAheadTarget: #block.
	] ifTrue: [ self emitMetaLinkInstead: aBlockNode ].
	self emitMetaLinkAfterNoEnsure: aBlockNode.
	

