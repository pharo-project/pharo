"
I am a semantic scope for the AST of method definition.
I define the visibility of the targetClass variables from the source code of the method.

My instances can be created using following expression:

```
	OCMethodSemanticScope targetingClass: aClass
```
"
Class {
	#name : 'OCMethodSemanticScope',
	#superclass : 'OCSemanticScope',
	#instVars : [
		'targetClass'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'instance creation' }
OCMethodSemanticScope class >> targetingClass: aClass [
	^self new
		targetClass: aClass
]

{ #category : 'accessing' }
OCMethodSemanticScope >> targetClass [

	^ targetClass
]

{ #category : 'accessing' }
OCMethodSemanticScope >> targetClass: anObject [

	targetClass := anObject
]
