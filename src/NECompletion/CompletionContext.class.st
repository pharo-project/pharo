"
I am a context class that hooks the new completion functionality into the system. To do that, i reimplement some of the methods but use most of the old ones from NECompletion package, such as the functionality to call the menu, etc.
"
Class {
	#name : #CompletionContext,
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
CompletionContext class >> controller: aNECController class: aClass source: aString position: anInteger [ 
	^ self new
		setController: aNECController
		class: aClass
		source: aString
		position: anInteger
]

{ #category : #accessing }
CompletionContext >> completionToken [

	"here we need to return the name of the var or selector"

	node ifNil: [ ^ '' ].
	^ completionToken ifNil: [ node completionToken ]
]

{ #category : #accessing }
CompletionContext >> createModel [
	self parseSource.
	node := ast nodeForOffset: position.
  ^ (CompletionModel new node: node) clazz: theClass
]

{ #category : #accessing }
CompletionContext >> model [
    ^model ifNil: [ model := self createModel ]
]

{ #category : #action }
CompletionContext >> narrowWith: aString [
	"method borrowed from NECContext to make the completion plugging work"
	completionToken := aString.
	model := self createModel.
	model narrowWith: aString.
]

{ #category : #parsing }
CompletionContext >> parseSource [

	ast := theClass
		ifNil: [ ( RBParser parseFaultyExpression: source ) doSemanticAnalysis ]
		ifNotNil: [ ( RBParser parseFaultyMethod: source ) doSemanticAnalysisIn: theClass ].
	TypingVisitor new visitNode: ast
]

{ #category : #accessing }
CompletionContext >> receiverClass [
	node isMessage ifFalse: [ ^nil ].
	^node receiver propertyAt: #type ifAbsent: [ nil ] .

]

{ #category : #'initialize-release' }
CompletionContext >> setController: aECController class: aClass source: aString position: anInteger [ 
	theClass := aClass. 
	source := aString.
	position := anInteger
]
