"
I am a scope for DoIt expressions targeting a given receiver (doIt/printIt in the inspector).

I implement a visibility of varibles defined in the target receiver:
- inst vars
- class vars 
- etc
Thus the doIt expressions can transparently access variables of target receiver.
In inspector it means that users can execute or debug the code containing instance and shared variables. 

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
