"
Boolean is an abstract class defining the protocol for logic testing operations and conditional control structures for the logical values represented by the instances of its subclasses True and False.

Boolean redefines #new so no instances of Boolean can be created. It also redefines several messages in the 'copying' protocol to ensure that only one instance of each of its subclasses True (the global true, logical assertion) and False (the global false, logical negation) ever exist in the system.
"
Class {
	#name : #Boolean,
	#superclass : #Object,
	#classVars : [
		'DeOptimizeMustBeBoolean'
	],
	#category : #'Kernel-Objects'
}

{ #category : #deoptimize }
Boolean class >> mustBeBooleanDeOptimize [
		^DeOptimizeMustBeBoolean ifNil: [ ^true ].
]

{ #category : #deoptimize }
Boolean class >> mustBeBooleanDeOptimize: aBoolean [
		DeOptimizeMustBeBoolean := aBoolean
]

{ #category : #'instance creation' }
Boolean class >> new [
	self error: 'You may not create any more Booleans - this is two-valued logic'
]

{ #category : #'logical operations' }
Boolean >> & aBoolean [ 
	"Evaluating conjunction. Evaluate the argument. Then answer true if 
	both the receiver and the argument are true."	
	"true & true >>> true"
	"true & false >>> false"
	"false & true >>> false"
	"false & false >>> false"

	self subclassResponsibility
]

{ #category : #'logical operations' }
Boolean >> ==> aBlock [
	"The material conditional, also known as the material implication or truth functional conditional.
	Correspond to not ... or ... and does not correspond to the English if...then... construction.
		
	 known as:
			b if a 
			a implies b
			if a then b
			b is a consequence of a
			a therefore b (but note: 'it is raining therefore it is cloudy' is implication; 'it is autumn therefore the leaves are falling' is equivalence).
		
	Here is the truth table for material implication:
	
	   p   |   q   |   p ==> q
	-------|-------|-------------
	   T   |   T   |      T
	   T   |   F   |      F
	   F   |   T   |      T
	   F   |   F   |      T
	"
	"true ==> [true] >>> true"
	"true ==> [false] >>> false"
	"false ==> [true] >>> true"
	"false ==> [false] >>> true"

	^ self not or: [ aBlock value ]
]

{ #category : #controlling }
Boolean >> and: alternativeBlock [ 
	"Nonevaluating conjunction. If the receiver is true, answer the value of 
	the argument, alternativeBlock; otherwise answer false without 
	evaluating the argument."
	
	"(true and: [true]) >>> true"
	"(true and: [false]) >>> false"
	"(false and: [true]) >>> false"
	"(false and: [Error signal]) >>> false"

	self subclassResponsibility
]

{ #category : #converting }
Boolean >> asBit [
	"convert myself to an Integer representing 1 for true and 0 for false"
	
	"($b > $c) asBit>>> 0"
	"true asBit >>> 1" 
	"(#(true true false true) inject: 0 into: [ :sum :each | sum + each asBit]) >>> 3"
	
	self subclassResponsibility
]

{ #category : #copying }
Boolean >> deepCopy [ 
	"Receiver has two concrete subclasses, True and False.
	Only one instance of each should be made, so return self."
]

{ #category : #'logical operations' }
Boolean >> eqv: aBoolean [
	"Answer true if the receiver is equivalent to aBoolean."

	"(false eqv: true) >>> false"
	"((1 == 1) eqv: true) >>> true"
	"(( 1 > 2)  eqv: false) >>> true"

	^ self == aBoolean
]

{ #category : #controlling }
Boolean >> ifFalse: alternativeBlock [ 
	"If the receiver is true (i.e., the condition is true), then the value is the 
	true alternative, which is nil. Otherwise answer the result of evaluating 
	the argument, alternativeBlock. Create an error notification if the 
	receiver is nonBoolean. Execution does not actually reach here because 
	the expression is compiled in-line."
	
	"(false ifFalse: [ 'This statement is false!' ]) >>> 'This statement is false!'" 
	"(true ifFalse: [ 'This statement is false!' ]) >>> nil"

	self subclassResponsibility
]

{ #category : #controlling }
Boolean >> ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock [ 
	"Same as ifTrue:ifFalse:."
	
	"(true ifFalse: [ 'That is false!' ] ifTrue: [ 'That is true!' ])  >>> 'That is true!'"
	"(false ifFalse: [ 'That is false!' ] ifTrue: [ 'That is true!' ])  >>> 'That is false!'"

	self subclassResponsibility
]

{ #category : #controlling }
Boolean >> ifTrue: alternativeBlock [ 
	"If the receiver is false (i.e., the condition is false), then the value is the 
	false alternative, which is nil. Otherwise answer the result of evaluating 
	the argument, alternativeBlock. Create an error notification if the 
	receiver is nonBoolean. Execution does not actually reach here because 
	the expression is compiled in-line."
	
	"(true ifTrue: [ 'This statement is true!' ]) >>> 'This statement is true!'" 
	"(false ifTrue: [ 'This statement is true!' ]) >>> nil"

	self subclassResponsibility
]

{ #category : #controlling }
Boolean >> ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock [
	"If the receiver is true (i.e., the condition is true), then answer the value 
	of the argument trueAlternativeBlock. If the receiver is false, answer the 
	result of evaluating the argument falseAlternativeBlock. If the receiver 
	is a nonBoolean then create an error notification. Execution does not 
	actually reach here because the expression is compiled in-line."
	
	"(true ifTrue: [ 'That is true!' ] ifFalse: [ 'That is false!' ]) >>> 'That is true!'"
	"(false ifTrue: [ 'That is true!' ] ifFalse: [ 'That is false!' ])  >>> 'That is false!'"

	self subclassResponsibility
]

{ #category : #printing }
Boolean >> isLiteral [ 
	^ true
]

{ #category : #'self evaluating' }
Boolean >> isSelfEvaluating [
	^ true
]

{ #category : #'logical operations' }
Boolean >> not [
	"Negation. Answer true if the receiver is false, answer false if the 
	receiver is true."
	"true not >>> false"
	"false not >>> true"

	self subclassResponsibility
]

{ #category : #controlling }
Boolean >> or: alternativeBlock [ 
	"Nonevaluating disjunction. If the receiver is false, answer the value of 
	the argument, alternativeBlock; otherwise answer true without 
	evaluating the argument."
	
	"(true or: [Error signal]) >>> true"
	"(true or: [false]) >>> true"
	"(false or: [true]) >>> true"
	"(false or: [3]) >>> 3"

	self subclassResponsibility
]

{ #category : #copying }
Boolean >> shallowCopy [ 
	"Receiver has two concrete subclasses, True and False.
	Only one instance of each should be made, so return self."
]

{ #category : #printing }
Boolean >> storeOn: aStream [ 
	"Refer to the comment in Object|storeOn:."

	self printOn: aStream
]

{ #category : #copying }
Boolean >> veryDeepCopyWith: deepCopier [
	"Return self.  I can't be copied.  Do not record me."
]

{ #category : #controlling }
Boolean >> xor: alternativeBlock [ 
	"Nonevaluating conjunction. If the receiver is true, answer the opposite of the 
	the argument, alternativeBlock; otherwise answer the value of the alternativeBlock."
	
	"(true xor: [true]) >>> false"
	"(true xor: [false]) >>> true"
	"(false xor: [true]) >>> true"
	"(false xor: [false]) >>> false"

	self subclassResponsibility
]

{ #category : #'logical operations' }
Boolean >> | aBoolean [ 
	"Evaluating disjunction (OR). Evaluate the argument. Then answer true 
	if either the receiver or the argument is true."
	
	"true | true >>> true"
	"true | false >>> true"
	"false | true >>> true"
	"false | false >>> false"

	self subclassResponsibility
]
