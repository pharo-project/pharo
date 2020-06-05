"
I'm a code completion engine.
I can have several configurations: 

- I have a completionClass, and I am not for scripting -> I will be used in a browser.
- I have a completionClass, and I am for scripting -> I will be used in an inspector.
- I do not have a completionClass, and I am not for scripting -> I am in a method without class
- I do not have a completionClass, and I am for scription -> I will be used in a workspace (playground)

"
Class {
	#name : #SpCodeCompletionEngine,
	#superclass : #CoCompletionEngine,
	#instVars : [
		'scripting',
		'completionClass',
		'doItContext',
		'doItRequestor'
	],
	#category : #'Spec2-Code'
}

{ #category : #accessing }
SpCodeCompletionEngine >> completionClass [
	^ completionClass
]

{ #category : #accessing }
SpCodeCompletionEngine >> completionClass: anObject [
	completionClass := anObject
]

{ #category : #accessing }
SpCodeCompletionEngine >> doItContext [

	^ doItContext
]

{ #category : #accessing }
SpCodeCompletionEngine >> doItContext: aContext [

	doItContext := aContext
]

{ #category : #accessing }
SpCodeCompletionEngine >> doItRequestor [

	^ doItRequestor
]

{ #category : #accessing }
SpCodeCompletionEngine >> doItRequestor: aRequestor [

	doItRequestor := aRequestor
]

{ #category : #testing }
SpCodeCompletionEngine >> isScripting [

	^ scripting
]

{ #category : #accessing }
SpCodeCompletionEngine >> scripting: aBoolean [ 

	scripting := aBoolean
]

{ #category : #accessing }
SpCodeCompletionEngine >> variableBindingNames [

	^ self doItRequestor variableBindingNames
]
