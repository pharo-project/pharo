Class {
	#name : #GLMSmalltalkCodeModel,
	#superclass : #GLMTextModel,
	#instVars : [
		'highlightSmalltalk',
		'highlightSmalltalkContext',
		'variableBindings',
		'bindingsStrategy'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #accessing }
GLMSmalltalkCodeModel >> addVariableBinding: anAssociation [
	self variableBindings at: anAssociation key put: anAssociation value
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> bindingOf: aSymbol [
	^ self bindingsStrategy cull: self variableBindings cull: aSymbol
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> bindingsStrategy [

	^ bindingsStrategy ifNil: [ bindingsStrategy := self defaultBindingsStrategy ]
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> bindingsStrategy: aBlock [

	bindingsStrategy := aBlock
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> defaultBindingsStrategy [

	^ [ :currentBindings :aSymbol |
		 currentBindings associationAt: aSymbol ifAbsent: [nil] ]
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> doItContext [
	^ self glamourPresentation doItContext 
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> doItReceiver [
	^ self glamourPresentation doItReceiver 
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> hasBindingOf: aSymbol [
	^ self variableBindings includesKey: aSymbol
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> hasBindingThatBeginsWith: aString [
	^ self variableBindings keys anySatisfy: [:each | each beginsWith: aString]
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> highlightSmalltalk [
	^ highlightSmalltalk
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> highlightSmalltalk: anObject [
	highlightSmalltalk := anObject
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> highlightSmalltalkContext [
	^ highlightSmalltalkContext
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> highlightSmalltalkContext: anObject [
	highlightSmalltalkContext := anObject
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> selectedClassOrMetaClass [
	^ self highlightSmalltalkContext
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> shoutAboutToStyle: aPluggableShoutMorph [
	self highlightSmalltalk ifFalse: [^ false].
	self highlightSmalltalkContext notNil ifTrue: [
		aPluggableShoutMorph classOrMetaClass: self highlightSmalltalkContext].
	^ true
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> variableBindings [
	^ variableBindings ifNil: [variableBindings := Dictionary new]
]

{ #category : #accessing }
GLMSmalltalkCodeModel >> variableBindings: anObject [
	variableBindings := anObject
]
