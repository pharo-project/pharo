Class {
	#name : #RBAbstractTransformation,
	#superclass : #Object,
	#instVars : [
		'model',
		'options'
	],
	#classVars : [
		'RefactoringOptions'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #displaying }
RBAbstractTransformation class >> basicMenuItemString [

	^ self subclassResponsibility
]

{ #category : #cleanup }
RBAbstractTransformation class >> cleanUp [
	"RefactoringOptions holds on to blocks, we should make sure to recreate them
	 so the block references the current method"
	self initializeRefactoringOptions
]

{ #category : #'class initialization' }
RBAbstractTransformation class >> initialize [
	self initializeRefactoringOptions
]

{ #category : #'private - initialization' }
RBAbstractTransformation class >> initializeRefactoringOptions [
	RefactoringOptions := IdentityDictionary new.
	RefactoringOptions
		at: #implementorToInline
		put: [ :ref :imps | self error: #implementorToInline ];

		at: #methodName
		put: [ :methodName :ref | self error: #methodName ];

		at: #selfArgumentName
		put: [ :ref | self error: #selfArgumentName ];

		at: #selectVariableToMoveTo
		put: [ :ref :class :selector | self error: #selectVariableToMoveTo ];

		at: #variableTypes
		put: [ :ref :types :selected | self error: #variableTypes ];

		at: #extractAssignment
		put: [ :ref :varName | self error: #extractAssignment ];

		at: #inlineExpression
		put: [ :ref :string | self error: #inlineExpression ];

		at: #alreadyDefined
		put: [ :ref :cls :selector |  self error: #alreadyDefined ];

		at: #useExistingMethod
		put: [ :ref :selector |  self error: #useExistingMethod ];

		at: #openBrowser
		put: [ :ref :env |  self error: #openBrowser ];

		at: #searchInWholeHierarchy
		put: [ self error: #searchInWholeHierarchy ]
]

{ #category : #displaying }
RBAbstractTransformation class >> menuItemString [

	^ (self isTransformation
		   ifTrue: [ '(T) ' ]
		   ifFalse: [ '' ]) , self basicMenuItemString
]

{ #category : #accessing }
RBAbstractTransformation class >> refactoringOptions [
	^ RefactoringOptions
]

{ #category : #accessing }
RBAbstractTransformation class >> setDefaultOption: aSymbol to: aBlock [
	self refactoringOptions at: aSymbol put: aBlock
]

{ #category : #'defining conditions' }
RBAbstractTransformation >> checkInstanceVariableName: aName in: aClass [
	^RBCondition checkInstanceVariableName: aName in: aClass
]

{ #category : #'defining conditions' }
RBAbstractTransformation >> checkMethodName: aName in: aClass [
	^RBCondition checkMethodName: aName in: aClass
]

{ #category : #'defining conditions' }
RBAbstractTransformation >> emptyCondition [
	^ RBCondition empty
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeRewriter [
	^ self parseTreeRewriterClass new
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeRewriterClass [
	^ RBParseTreeRewriter
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeSearcher [
	^ self parseTreeSearcherClass new
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeSearcherClass [
	^ RBParseTreeSearcher
]

{ #category : #parsing }
RBAbstractTransformation >> parserClass [
	^ RBParser
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringConfirmWarning: aString [
	| ret |
	ret := self uiManager
		confirm: aString
		orCancel: [ ^ RBRefactoringError signal: 'aborted' ].
	^ ret
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringError: aString [
	^ RBRefactoringError signal: aString
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringWarning: aString [
	^ RBRefactoringWarning signal: aString
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringWarning: aString with: aBlock [
	^ RBRefactoringWarning signal: aString with: aBlock
]
