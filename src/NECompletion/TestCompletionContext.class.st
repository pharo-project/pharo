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
		'ast',
		'node'
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

	node ifNil: [ ^ '' ].
	^ completionToken ifNil: [ node completionToken ]
]

{ #category : #accessing }
TestCompletionContext >> createModel [
	self parseSource.
	node := ast bestNodeFor: (position to: position).
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
	model := self createModel.
	model narrowWith: aString.
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
	^node receiver propertyAt: #type ifAbsent: [ nil ] .

]

{ #category : #'initialize-release' }
TestCompletionContext >> setController: aECController class: aClass source: aString position: anInteger [ 
	theClass := aClass. 
	source := aString.
	position := anInteger
]
