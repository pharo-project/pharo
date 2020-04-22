Class {
	#name : #CoInitializeTypeInferenceTest,
	#superclass : #TestCase,
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
CoInitializeTypeInferenceTest >> testInferVariablesWithNoInitializeFindsNoTypes [

	| types |
	types := CoTypeInferencer new
		inferFrom: (CoMockClass new
			instanceVariables: #(a b c);
			yourself);
		variables.
		
	self assert: (types values allSatisfy: [ :e | e isEmpty ]).
]
