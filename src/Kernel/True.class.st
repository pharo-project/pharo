"
True defines the behavior of its single instance, true -- logical assertion. Notice how the truth-value checks become direct message sends, without the need for explicit testing.

Be aware however that most of these methods are not sent as real messages in normal use. Most are inline coded by the compiler as test and jump bytecodes - avoiding the overhead of the full message sends. So simply redefining these methods here will have no effect.
"
Class {
	#name : #True,
	#superclass : #Boolean,
	#category : #'Kernel-Objects'
}

{ #category : #'logical operations' }
True >> & aBoolean [ 
	"Evaluating conjunction -- answer aBoolean since receiver is true."

	^aBoolean
]

{ #category : #controlling }
True >> and: alternativeBlock [ 
	"Nonevaluating conjunction -- answer the value of alternativeBlock since
	the receiver is true."
	"(true and: [ false ]) >>> false"
	"(true and: [ true ]) >>> true"

	^ alternativeBlock value
]

{ #category : #converting }
True >> asBit [

	^ 1
]

{ #category : #controlling }
True >> ifFalse: alternativeBlock [ 
	"Since the condition is true, the value is the true alternative, which is nil. 
	Execution does not actually reach here because the expression is compiled 
	in-line."

	^nil
]

{ #category : #controlling }
True >> ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock [ 
	"Answer the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value
]

{ #category : #controlling }
True >> ifTrue: alternativeBlock [ 
	"Answer the value of alternativeBlock. Execution does not actually 
	reach here because the expression is compiled in-line."

	^alternativeBlock value
]

{ #category : #controlling }
True >> ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock [ 
	"Answer with the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value
]

{ #category : #'logical operations' }
True >> not [
	"Negation--answer false since the receiver is true."

	^false
]

{ #category : #controlling }
True >> or: alternativeBlock [ 
	"Nonevaluating disjunction -- answer true since the receiver is true."
	"(true or: [ 1 error ])>>> true"
	
	^ self
]

{ #category : #printing }
True >> printOn: aStream [ 

	aStream nextPutAll: 'true'
]

{ #category : #'logical operations' }
True >> xor: alternativeBlock [
	"Nonevaluating conjunction. If the receiver is true, answer the opposite of the 
	the argument, alternativeBlock; otherwise answer the value of the alternativeBlock."
	"(true xor: [ true ]) >>> false"
	
	^ alternativeBlock value not
]

{ #category : #'logical operations' }
True >> | aBoolean [ 
	"Evaluating disjunction (OR) -- answer true since the receiver is true."

	^self
]
