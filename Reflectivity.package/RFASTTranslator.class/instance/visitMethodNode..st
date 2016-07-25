visitor-double dispatching
visitMethodNode: aMethodNode
	"I ignore all links when I am primitive as ReflectiveMethod compiles a wrapper"
	
	aMethodNode arguments size > 15 ifTrue: [self backendError: 'Too many arguments' forNode: aMethodNode ].
	
	methodBuilder compilationContext: aMethodNode compilationContext.
	methodBuilder addTemps: aMethodNode scope tempVarNames.
	
	aMethodNode isPrimitive ifFalse: [self emitMetaLinkBefore: aMethodNode].

	methodBuilder properties: aMethodNode properties.
	methodBuilder irPrimitive: aMethodNode primitiveFromPragma.
	aMethodNode pragmas do: [:each | self visitPragmaNode: each].
	methodBuilder numArgs: aMethodNode arguments size.
	
	self emitPrepareLinkAfter: aMethodNode.
	
	(aMethodNode hasMetalinkInstead and: [ aMethodNode isPrimitive not ])
				ifTrue: [ self emitMetaLinkInstead: aMethodNode. methodBuilder returnTop.
						^self. ].
				
	aMethodNode scope tempVector ifNotEmpty: [
		methodBuilder 
			createTempVectorNamed: aMethodNode scope tempVectorName 
			withVars: (aMethodNode scope tempVector collect: [:each| each name]) asArray.
	].
	effectTranslator visitNode: aMethodNode body.
	aMethodNode body lastIsReturn ifFalse:  [methodBuilder pushReceiver; returnTop].
	self emitMetaLinkAfter: aMethodNode.
	aMethodNode hasMetalinkAfter ifTrue: [methodBuilder returnTop].
