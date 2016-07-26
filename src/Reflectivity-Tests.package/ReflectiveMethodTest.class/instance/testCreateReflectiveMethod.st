tests - creation
testCreateReflectiveMethod
	| compiledMethod reflectiveMethod |
	compiledMethod := ReflectivityExamples>>#exampleMethod.
	reflectiveMethod := ReflectiveMethod on: compiledMethod.
	self assert: reflectiveMethod class = ReflectiveMethod.