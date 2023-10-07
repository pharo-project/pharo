Class {
	#name : 'CoInitializeTypeInferenceTest',
	#superclass : 'TestCase',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
CoInitializeTypeInferenceTest >> testInferInstanceVariableNotUsedInInitializeHasNoType [

	| types |
	types := CoTypeInferencer new
		inferFrom: (CoMockClass new
			methodAt: #initialize put: (CoMockMethod new
				source: 'initialize
					a := 1'
				yourself);
			instanceVariables: #(a b c);
			yourself);
		variables.

	self assert: (types at: #b) isEmpty
]

{ #category : 'tests' }
CoInitializeTypeInferenceTest >> testInferInstanceVariableUsedInInitialize [

	| types |
	types := CoTypeInferencer new
		inferFrom: (CoMockClass new
			methodAt: #initialize put: (CoMockMethod new
				source: 'initialize
					a := 1'
				yourself);
			instanceVariables: #(a b c);
			yourself);
		variables.

	self assertCollection: (types at: #a) hasSameElements: { SmallInteger }
]

{ #category : 'tests' }
CoInitializeTypeInferenceTest >> testInferUndefinedInstanceVariableUsedInInitialize [

	| types |
	types := CoTypeInferencer new
		inferFrom: (CoMockClass new
			methodAt: #initialize put: (CoMockMethod new
				source: 'initialize
					a'
				yourself);
			instanceVariables: #();
			yourself);
		variables.

	self assertCollection: (types at: #a) hasSameElements: Set new
]

{ #category : 'tests' }
CoInitializeTypeInferenceTest >> testInferVariablesWithNoInitializeFindsNoTypes [

	| types |
	types := CoTypeInferencer new
		inferFrom: (CoMockClass new
			instanceVariables: #(a b c);
			yourself);
		variables.

	self assert: (types values allSatisfy: [ :e | e isEmpty ])
]
