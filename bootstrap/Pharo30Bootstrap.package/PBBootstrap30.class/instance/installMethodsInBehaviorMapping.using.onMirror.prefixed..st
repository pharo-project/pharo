running
installMethodsInBehaviorMapping: aBehaviorMapping using: aMethodBuilder onMirror: mirror prefixed: aPrefix
	| newMethods theClass |
	newMethods := aMethodBuilder methodsFromDefinition: aBehaviorMapping.
	newMethods := newMethods sorted: [ :m1 :m2 | m1 selector < m2 selector ].
	theClass := (self classNamed: aBehaviorMapping theNonMetaClass name) mirror.
	aBehaviorMapping isMeta ifTrue: [ theClass := theClass basicClass ].
	newMethods do: [ :m |
		self log: aPrefix, ': Installing method ', 
			aBehaviorMapping name, '>>', m selector.
		self objectSpace interpreter
			evaluateCode: 'theBehavior addSelectorSilently: selector withMethod: compiledMethod'
			withTemps: { 
				'theBehavior' -> theClass.
				'selector' -> (m selector asLiteralInObjectSpace: objectSpace).
				'compiledMethod' -> m }
	].