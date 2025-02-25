"
False defines the behavior of its single instance, false -- logical negation. Notice how the truth-value checks become direct message sends, without the need for explicit testing.

Be aware however that most of these methods are not sent as real messages in normal use. Most are inline coded by the compiler as test and jump bytecodes - avoiding the overhead of the full message sends. So simply redefining these methods here will have no effect.
"
Class {
	#name : 'False',
	#superclass : 'Boolean',
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'logical operations' }
False >> & aBoolean [
	"Evaluating conjunction -- answer false since receiver is false."
	"false & true >>> false"
	"false & false >>> false"

	^self
]

{ #category : 'controlling' }
False >> and: alternativeBlock [
	"Nonevaluating conjunction -- answer with false since the receiver is false."
	"(false and: [true]) >>> false"
	"(false and: [Error signal]) >>> false"

	^self
]

{ #category : 'converting' }
False >> asBit [
	"Answer 0 since the receiver is false."
	"false asBit >>> 0"

	^ 0
]

{ #category : 'controlling' }
False >> ifFalse: alternativeBlock [
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."
	"(false ifFalse: [ 'This statement is false!' ]) >>> 'This statement is false!'"

	^alternativeBlock value
]

{ #category : 'controlling' }
False >> ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock [
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."
	"(false ifFalse: [ 'That is false!' ] ifTrue: [ 'That is true!' ])  >>> 'That is false!'"

	^falseAlternativeBlock value
]

{ #category : 'controlling' }
False >> ifTrue: alternativeBlock [
	"Since the condition is false, answer the value of the false alternative,
	which is nil. Execution does not actually reach here because the
	expression is compiled in-line."
	"(false ifTrue: [ 'This statement is true!' ]) >>> nil"

	^nil
]

{ #category : 'controlling' }
False >> ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock [
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."
	"(false ifTrue: [ 'That is true!' ] ifFalse: [ 'That is false!' ])  >>> 'That is false!'"

	^falseAlternativeBlock value
]

{ #category : 'logical operations' }
False >> not [
	"Negation -- answer true since the receiver is false."
	"false not >>> true"

	^true
]

{ #category : 'controlling' }
False >> or: alternativeBlock [
	"Nonevaluating disjunction -- answer value of alternativeBlock."
	"(false or: [true]) >>> true"
	"(false or: [3]) >>> 3"

	^alternativeBlock value
]

{ #category : 'printing' }
False >> printOn: aStream [

	aStream nextPutAll: 'false'
]

{ #category : 'logical operations' }
False >> xor: alternativeBlock [
	"Answer the value of the alternativeBlock since the receiver is false."
	"(false xor: [true]) >>> true"
	"(false xor: [false]) >>> false"

	^ alternativeBlock value
]

{ #category : 'logical operations' }
False >> | aBoolean [
	"Evaluating disjunction (OR) -- answer with the argument, aBoolean, since the receiver is false."
	"false | true >>> true"
	"false | false >>> false"

	^aBoolean
]
