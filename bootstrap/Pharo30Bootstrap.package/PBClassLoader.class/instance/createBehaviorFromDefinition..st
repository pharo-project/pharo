class-loading
createBehaviorFromDefinition: aRFiDefinition

	| newBehavior |
	(self isFullyLoadedClassNamed: aRFiDefinition name) ifTrue: [ ^ self ].
	
	aRFiDefinition isBehavior ifTrue: [
		"We ensure a class with that name already exists"
		environment at: aRFiDefinition name ifAbsent: [ self createStubForClassNamed: aRFiDefinition name ]].
	
	newBehavior := self executeDefinitionFor: aRFiDefinition.
	
	"Once the class is created we set a new method dictionary with a size that will make it grow less afterwards"
	builder bootstrapInterpreter 
		evaluateCode: 'theBehavior methodDictionary: (MethodDictionary new: capacity)'
		withTemps: { 
			'capacity' -> (aRFiDefinition methods size asLiteralInObjectSpace: builder objectSpace).
			'theBehavior' -> newBehavior }.
	
	aRFiDefinition isBehavior ifTrue: [ 
		self registerClass: newBehavior asClassMirror named: aRFiDefinition name fullyLoaded: true.
		FileStream stdout nextPutAll: 'behavior: ',aRFiDefinition name; cr.
	] ifFalse: [
		self registerTrait: newBehavior asTraitMirror named: aRFiDefinition name fullyLoaded: true.
		FileStream stdout nextPutAll: 'trait',aRFiDefinition name; cr.
	]