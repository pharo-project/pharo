"
I am a root scope for the AST of method definition (compiled in the browser for example).

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
	^self asDoItForReceiver: nil
]

{ #category : #'code compilation' }
OCMethodSemanticScope >> buildASTBy: aParser [

	| ast |
	ast := aParser parseMethod.
	ast semanticScope: self.
	^ast
]

{ #category : #'code evaluation' }
OCMethodSemanticScope >> evaluateDoIt: doItMethod [

	^nil withArgs: #() executeMethod: doItMethod
]

{ #category : #accessing }
OCMethodSemanticScope >> targetClass [

	^ targetClass
]

{ #category : #accessing }
OCMethodSemanticScope >> targetClass: anObject [

	targetClass := anObject
]

{ #category : #'code compilation' }
OCMethodSemanticScope >> transformDoItExpressionToMethodAST: expressionAST [
	^expressionAST asDoit
]
