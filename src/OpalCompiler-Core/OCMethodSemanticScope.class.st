"
I am a semantic scope for the AST of method definition.

I define the visibility of #targetClass variables from the source code of the method.

My instances can be created using following expression:

	OCMethodSemanticScope targetingClass: aClass
"
Class {
	#name : #OCMethodSemanticScope,
	#superclass : #OCSemanticScope,
	#instVars : [
		'targetClass'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'instance creation' }
OCMethodSemanticScope class >> targetingClass: aClass [ 
	^self new 
		targetClass: aClass
]

{ #category : #converting }
OCMethodSemanticScope >> asDoItScope [
	^self asDoItScopeForReceiver: nil
]

{ #category : #'code compilation' }
OCMethodSemanticScope >> compileMethodFromASTBy: aCompiler [
	^aCompiler compileMethodFromAST
]

{ #category : #'code compilation' }
OCMethodSemanticScope >> parseASTBy: aParser [

	^aParser parseMethod
]

{ #category : #accessing }
OCMethodSemanticScope >> targetClass [

	^ targetClass
]

{ #category : #accessing }
OCMethodSemanticScope >> targetClass: anObject [

	targetClass := anObject
]
