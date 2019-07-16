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
	#category : #'NECompletion-New'
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
	node isNil ifTrue: [ ^ '' ].
	^completionToken
		ifNil: [ node completionToken ]
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
	^ (TestCompletionModel new node: node) clazz: theClass
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
	ast := (theClass isNil)
        ifTrue: [ (RBParser parseFaultyExpression: source) doSemanticAnalysis]
        ifFalse: [ (RBParser parseFaultyMethod: source) doSemanticAnalysisIn: theClass]. 
    TestTypingVisitor new visitNode: ast.
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
