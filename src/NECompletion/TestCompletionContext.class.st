"
I am a context class that hooks the new completion functionality into the system. To do that, i reimplement some of the methods but use most of the old ones from NECompletion package, such as the functionality to call the menu, etc.
"
Class {
	#name : #TestCompletionContext,
	#superclass : #Object,
	#instVars : [
		'source',
		'position',
		'theClass',
		'completionToken',
		'model',
		'receiverClass',
		'controller',
		'ast',
		'node',
		'variables',
		'selectors',
		'context'
	],
	#category : #'NECompletion-ExperimentalCompletion'
}

{ #category : #'instance creation' }
TestCompletionContext class >> controller: aNECController class: aClass source: aString position: anInteger [ 
	^ self new
		setController: aNECController
		class: aClass
		source: aString
		position: anInteger
]

{ #category : #accessing }
TestCompletionContext >> completionToken [

	"here we need to return the name of the var or selector"

	node ifNil: [ ^ '' ].
	^ completionToken ifNil: [ node completionToken ]
]

{ #category : #action }
TestCompletionContext >> configureSelectorsAndVariables [
	variables := true.
	selectors := true.
	^true
]

{ #category : #accessing }
TestCompletionContext >> context: aCompletionContext [
	context := aCompletionContext.
]

{ #category : #accessing }
TestCompletionContext >> createModel [

	node ifNil: [ ^ NECEmptyModel new ].
	node isLiteralNode
		ifTrue: [ ^ NECEmptyModel new ].
	node isSequence
		ifTrue: [ ^ NECEmptyModel new ].
	node isMethod
		ifTrue: [ ^ NECEmptyModel new ].
	self receiverClass ifNotNil: [ ^ NECTypedModel class: receiverClass ].
	variables := false.
	selectors := true.
	node isMessage
		ifFalse: [ variables := true.
			selectors := true
			].

	^ NECUntypedModel
		class: theClass
		temporaries:
			( ast isMethod
				ifTrue: [ ast temporaryNames collect: [ :each | NECLocalEntry contents: each ] ]
				ifFalse: [ #() ] )
		additionals: #()
		variables: variables
		selectors: selectors
]

{ #category : #accessing }
TestCompletionContext >> model [
    ^model ifNil: [ model := self createModel ]
]

{ #category : #action }
TestCompletionContext >> narrowWith: aString [
	"method borrowed from NECContext to make the completion plugging work"
	completionToken := aString.
	model ifNotNil: [ model narrowWith: aString].
]

{ #category : #parsing }
TestCompletionContext >> parseSource [

	ast := theClass
		ifNil: [ ( RBParser parseFaultyExpression: source ) doSemanticAnalysis ]
		ifNotNil: [ ( RBParser parseFaultyMethod: source ) doSemanticAnalysisIn: theClass ].
	TestTypingVisitor new visitNode: ast
]

{ #category : #accessing }
TestCompletionContext >> receiverClass [
	node isMessage ifFalse: [ ^nil ].
	receiverClass := node receiver propertyAt: #type ifAbsent: [ nil ] .
	^ receiverClass
]

{ #category : #'initialize-release' }
TestCompletionContext >> setController: aECController class: aClass source: aString position: anInteger [ 
	controller := aECController.
	theClass := aClass.
	source := aString.
	position := anInteger. 
	self parseSource.
	node := ast bestNodeFor: (position to: position).
	
]
