"
A trait to setUp refactorings for tests purposes
"
Trait {
	#name : #TSetUpRefactorings,
	#category : #'Refactoring-DataForTesting'
}

{ #category : #'set up' }
TSetUpRefactorings >> setupExtractionClassFor: aRefactoring toReturn: aClass [
	| options |
	options := aRefactoring options copy.
	options at: #extractionClass
		put: [:ref | aClass].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupImplementorToInlineFor: aRefactoring toReturn: anObject [
	| options |
	options := aRefactoring options copy.
	options at: #implementorToInline put: [:ref :imps | anObject].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupInlineExpressionFor: aRefactoring toReturn: aBoolean [
	| options |
	options := aRefactoring options copy.
	options at: #inlineExpression put: [:ref :string | aBoolean].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupMethodNameFor: aRefactoring toReturn: aSelector [
	| options |
	options := aRefactoring options copy.
	options at: #methodName
		put:
			[:aMethodName :ref |
			aMethodName
				selector: aSelector;
				yourself].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupMethodNameFor: aRefactoring toReturn: aSelector withArguments: stringCollection [
	| options |
	options := aRefactoring options copy.
	options at: #methodName
		put:
			[ :aMethodName :ref |
			aMethodName
				selector: aSelector;
				arguments: stringCollection;
				yourself].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupSearchInAllHierarchyFor: aRefactoring toReturn: aBoolean [
	| options |
	options := aRefactoring options copy.
	options at: #searchInWholeHierarchy put: [ :ref :cls | aBoolean ].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupSelfArgumentNameFor: aRefactoring toReturn: aString [
	| options |
	options := aRefactoring options copy.
	options at: #selfArgumentName put: [:ref | aString].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupShouldUseExistingMethodFor: aRefactoring toReturn: aBoolean [
	| options |
	options := aRefactoring options copy.
	options at: #useExistingMethod put: [ :ref :cls | aBoolean ].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupVariableToMoveToFor: aRefactoring toReturn: aString [
	| options |
	options := aRefactoring options copy.
	options at: #selectVariableToMoveTo put: [:ref :class :selector | aString].
	aRefactoring options: options
]

{ #category : #'set up' }
TSetUpRefactorings >> setupVariableTypesFor: aRefactoring toReturn: anObject [
	| options |
	options := aRefactoring options copy.
	options at: #variableTypes put: [:ref :ignore1 :ignore2 | anObject].
	aRefactoring options: options
]
