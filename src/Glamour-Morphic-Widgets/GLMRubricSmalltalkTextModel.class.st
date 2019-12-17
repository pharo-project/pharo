Class {
	#name : #GLMRubricSmalltalkTextModel,
	#superclass : #GLMRubricTextModel,
	#instVars : [
		'highlightSmalltalk',
		'highlightSmalltalkContext',
		'bindingsStrategy',
		'variableBindings'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> appendVariableBinding: anAssociationList [

	anAssociationList ifNotNil: [
		anAssociationList do: [ :anAssociation | 
			self variableBindings add: 
				((anAssociation class = WorkspaceVariable) 
					ifTrue: [ anAssociation ] 
					ifFalse: [ (WorkspaceVariable key: anAssociation key value: anAssociation value) ]) ] ]
]

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> bindingOf: aSymbol [

	^ self bindingsStrategy cull: self variableBindings cull: aSymbol cull: self
]

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> bindingsStrategy [

	^ bindingsStrategy
]

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> bindingsStrategy: aBlock [

	bindingsStrategy := aBlock
]

{ #category : #accessing }
GLMRubricSmalltalkTextModel >> doItContext [
	^ self glamourPresentation doItContext 
]

{ #category : #accessing }
GLMRubricSmalltalkTextModel >> doItReceiver [
	^ self glamourPresentation doItReceiver 
]

{ #category : #formatting }
GLMRubricSmalltalkTextModel >> formatSourceCode [
	self glamourPresentation formatSourceCode .
]

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> hasBindingOf: aSymbol [

	^ self variableBindings includesKey: aSymbol
]

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> hasBindingThatBeginsWith: aString [
	^ self variableBindings keys anySatisfy: [:each | each beginsWith: aString]
]

{ #category : #shout }
GLMRubricSmalltalkTextModel >> highlightSmalltalk [
	^ highlightSmalltalk
]

{ #category : #shout }
GLMRubricSmalltalkTextModel >> highlightSmalltalk: aBoolean [
	highlightSmalltalk := aBoolean
]

{ #category : #shout }
GLMRubricSmalltalkTextModel >> highlightSmalltalkContext [
	^ highlightSmalltalkContext
]

{ #category : #shout }
GLMRubricSmalltalkTextModel >> highlightSmalltalkContext: aClassOrMetaclass [
	highlightSmalltalkContext := aClassOrMetaclass
]

{ #category : #accessing }
GLMRubricSmalltalkTextModel >> selectedBehavior [
	
	^ self highlightSmalltalkContext
]

{ #category : #accessing }
GLMRubricSmalltalkTextModel >> selectedClassOrMetaClass [
	
	^ self selectedBehavior
]

{ #category : #accessing }
GLMRubricSmalltalkTextModel >> selectedMethod [

	^ self doItContext ifNil: [ nil ] ifNotNil: [ :aContext | aContext method ]
]

{ #category : #shout }
GLMRubricSmalltalkTextModel >> shoutAboutToStyle: aPluggableShoutMorph [
	self highlightSmalltalk ifFalse: [^ false].
	self highlightSmalltalkContext ifNotNil: [ :classOrMetaClass |
 		aPluggableShoutMorph classOrMetaClass: classOrMetaClass  ].
	^ true
]

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> variableBindings [
	^ variableBindings ifNil: [variableBindings := Dictionary new]
]

{ #category : #bindings }
GLMRubricSmalltalkTextModel >> variableBindings: aDictionary [
	variableBindings := aDictionary
]
