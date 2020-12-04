"
I'm a code completion engine.
I can have several configurations: 

- I have a completionClass, and I am not for scripting -> I will be used in a browser.
- I have a completionClass, and I am for scripting -> I will be used in an inspector.
- I do not have a completionClass, and I am not for scripting -> I am in a method without class
- I do not have a completionClass, and I am for scripting -> I will be used in a workspace (playground)

"
Class {
	#name : #SpMorphicCodeCompletionEngine,
	#superclass : #CompletionEngine,
	#instVars : [
		'scripting',
		'completionClass',
		'doItContext',
		'doItRequestor',
		'completionBuilder'
	],
	#category : #'Spec2-Code-Morphic'
}

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> completionBuilder [

	^ completionBuilder ifNil: [ CoASTHeuristicsResultSetBuilder new ]
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> completionBuilder: anObject [

	completionBuilder := anObject
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> completionClass [

	^ completionClass
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> completionClass: anObject [

	completionClass := anObject
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> doItContext [

	^ doItContext
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> doItContext: aContext [

	doItContext := aContext
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> doItRequestor [

	^ doItRequestor
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> doItRequestor: aRequestor [

	doItRequestor := aRequestor
]

{ #category : #initialization }
SpMorphicCodeCompletionEngine >> initialize [

	super initialize.
	contextClass := CoCompletionContext
]

{ #category : #testing }
SpMorphicCodeCompletionEngine >> isScripting [

	^ scripting
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> scripting: aBoolean [ 

	scripting := aBoolean
]

{ #category : #accessing }
SpMorphicCodeCompletionEngine >> variableBindingNames [

	^ self doItRequestor variableBindingNames
]
