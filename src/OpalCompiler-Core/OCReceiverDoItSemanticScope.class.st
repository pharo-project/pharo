"
I am a root scope for any DoIt expression evaluated for given receiver object (doIt in the inspector).

I allow scripts to see instance and shared varibales of the class of #targetReceiver.

My instances can be created using following expression:

	OCReceiverDoItSemanticScope targetingReceiver: anObject
"
Class {
	#name : #OCReceiverDoItSemanticScope,
	#superclass : #OCDoItSemanticScope,
	#instVars : [
		'targetReceiver'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'instance creation' }
OCReceiverDoItSemanticScope class >> targetingNilReceiver [ 
	^self targetingReceiver: nil
]

{ #category : #'instance creation' }
OCReceiverDoItSemanticScope class >> targetingReceiver: anObject [
	^self new 
		targetReceiver: anObject
]

{ #category : #'code compilation' }
OCReceiverDoItSemanticScope >> buildASTBy: aParser [

	| ast |
	ast := aParser parseExpression.
	ast := ast asDoit.
	ast semanticScope: self.
	^ast
	
]

{ #category : #'code evaluation' }
OCReceiverDoItSemanticScope >> evaluateDoIt: doItMethod [

	^targetReceiver withArgs: #() executeMethod: doItMethod
]

{ #category : #accessing }
OCReceiverDoItSemanticScope >> receiver [

	^ targetReceiver
]

{ #category : #accessing }
OCReceiverDoItSemanticScope >> receiver: anObject [

	targetReceiver := anObject
]

{ #category : #accessing }
OCReceiverDoItSemanticScope >> targetClass [ 
	^targetReceiver class
]

{ #category : #accessing }
OCReceiverDoItSemanticScope >> targetReceiver [

	^ targetReceiver
]

{ #category : #accessing }
OCReceiverDoItSemanticScope >> targetReceiver: anObject [

	targetReceiver := anObject
]

{ #category : #'code compilation' }
OCReceiverDoItSemanticScope >> transformDoItExpressionToMethodAST: expressionAST [
	^expressionAST asDoit
]
