running
installExtensionMethodsOf: aPackageDefinition prefixed: aPrefix
	aPackageDefinition extensionMethods
		doWithIndex: [ :methodDefinition :index | | compiledMethod behaviorMirror methodClass logMessage |
			methodClass :=  methodDefinition  methodClass.
			compiledMethod := self methodBuilder createNewMethod: methodDefinition.
			behaviorMirror := bootstrapEnvironment at:  methodClass theNonMetaClass name.
			methodClass isMeta ifTrue: [ behaviorMirror := behaviorMirror classSide ].
			
		logMessage := String streamContents: [ :stream |
			stream
				nextPutAll: aPrefix;
				nextPut: $-;
				nextPutAll: index asString;
				nextPut: $/;
				nextPutAll: aPackageDefinition extensionMethods size asString;
				nextPutAll: ': Installing extension method ';
				nextPutAll: methodClass name;
				nextPutAll: '>>';
				nextPutAll: methodDefinition selector ].
		self log: logMessage.
			
		self objectSpace interpreter
			evaluateCode: 'theBehavior addSelectorSilently: selector withMethod: compiledMethod'
			withTemps: { 
				'theBehavior' -> behaviorMirror.
				'selector' -> (methodDefinition selector asLiteralInObjectSpace: objectSpace).
				'compiledMethod' -> compiledMethod } ]