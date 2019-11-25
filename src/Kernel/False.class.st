"
False defines the behavior of its single instance, false -- logical negation. Notice how the truth-value checks become direct message sends, without the need for explicit testing.

Be aware however that most of these methods are not sent as real messages in normal use. Most are inline coded by the compiler as test and jump bytecodes - avoiding the overhead of the full message sends. So simply redefining these methods here will have no effect.
"
Class {
	#name : #False,
	#superclass : #Boolean,
	#category : #'Kernel-Objects'
}

{ #category : #'logical operations' }
False >> & aBoolean [ 
	"Evaluating conjunction -- answer false since receiver is false."

	^self
]

{ #category : #controlling }
False >> and: alternativeBlock [ 
	"Nonevaluating conjunction -- answer with false since the receiver is false."
	"(false and: [ 1 error ])>>> false"
	
	^self
]

{ #category : #converting }
False >> asBit [

	^ 0
]

{ #category : #controlling }
False >> ifFalse: alternativeBlock [ 
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."

	^alternativeBlock value
]

{ #category : #controlling }
False >> ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock [ 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value
]

{ #category : #controlling }
False >> ifTrue: alternativeBlock [ 
	"Since the condition is false, answer the value of the false alternative, 
	which is nil. Execution does not actually reach here because the
	expression is compiled in-line."

	^nil
]

{ #category : #controlling }
False >> ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock [ 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value
]

{ #category : #'logical operations' }
False >> not [
	"Negation -- answer true since the receiver is false."

	^true
]

{ #category : #controlling }
False >> or: alternativeBlock [ 
	"Nonevaluating disjunction -- answer value of alternativeBlock."
	" (false or: [ true ] )>>> true "
	
	^alternativeBlock value
]

{ #category : #printing }
False >> printOn: aStream [ 

	aStream nextPutAll: 'false'
]

{ #category : #'logical operations' }
False >> xor: alternativeBlock [
	"Nonevaluating conjunction. If the receiver is true, answer the opposite of the 
	the argument, alternativeBlock; otherwise answer the value of the alternativeBlock."
	"(false xor: [ true ]) >>> true"
	
	^ alternativeBlock value
]

{ #category : #'logical operations' }
False >> | aBoolean [ 
	"Evaluating disjunction (OR) -- answer with the argument, aBoolean."

	^aBoolean
]
