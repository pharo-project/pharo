"
I verify whether a method is invoked by any other method in the model, excluding those that belong to my candidate set.
Both my candidates and violators are represented as associations of the form class → method.
"
Class {
	#name : 'ReMethodsHaveSendersCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'classSelectorsMapping'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing - private' }
ReMethodsHaveSendersCondition >> candidates [
	| result |
	result := #() asSet.
	classSelectorsMapping keysAndValuesDo: [ :rbClass :selectorsList | 
		selectorsList do: [ :selector | result add: (rbClass -> (rbClass methodFor: selector)) ] ].
	^ result
]

{ #category : 'accessing - private' }
ReMethodsHaveSendersCondition >> classSelectorsMapping: aDictionary [

	classSelectorsMapping := aDictionary
]

{ #category : 'accessing' }
ReMethodsHaveSendersCondition >> model: aRBNamespace [
	"I think this method should be defined in ReReifiedCondition."
	model := aRBNamespace 
]

{ #category : 'accessing' }
ReMethodsHaveSendersCondition >> nonViolators [

	| nonViolators |
	" It is easier to find nonViolators, so I compute violators as the difference between candidates and nonViolators "
	nonViolators := OrderedCollection new.
	self candidates do: [ :assocClassMethod |
			| method |
			method := assocClassMethod value.
			model allReferencesTo: method selector do: [ :aRBMethod |
					| isInternalSender |
					isInternalSender := self candidates
						                    inject: false
						                    into: [ :bool :assoc | bool or: [ self same: assoc as: aRBMethod methodClass -> aRBMethod ] ].
					isInternalSender ifFalse: [ nonViolators add: assocClassMethod ] ] ].
	^ nonViolators
]

{ #category : 'private' }
ReMethodsHaveSendersCondition >> same: candidate1 as: candidate2 [
	" My candidates are complex and cannot be compared using `=` "
	^ candidate1 key = candidate2 key and: [ candidate1 value selector = candidate2 value selector ]
]

{ #category : 'private' }
ReMethodsHaveSendersCondition >> sameContents: someCandidates as: otherCandidates [

	someCandidates do: [ :c1 | otherCandidates detect: [ :c2 | self same: c1 as: c2 ] ifFound: [ ^ false ] ].
	^ true
]

{ #category : 'displaying' }
ReMethodsHaveSendersCondition >> violationMessageOn: aStream violators: theViolators [
	| verb |
	(self sameContents: self violators as: theViolators) 
		ifTrue: [ verb := 'don''t have' ]
		ifFalse: [ self assert: (self sameContents: theViolators as: self nonViolators). verb := 'have' ].
	theViolators ifEmpty: [ ^ self ] ifNotEmpty: [
			aStream nextPutAll: 'Method(s) '.
			aStream nextPutAll: verb.
			aStream nextPutAll: ' senders: '.
			theViolators do: [ :violator |
				aStream
					nextPutAll: violator asString;
					nextPutAll: ';';
					space ] ]
]

{ #category : 'accessing' }
ReMethodsHaveSendersCondition >> violators [
	| nonViolators |
	
	violators ifNotNil: [ ^ violators ].
	nonViolators := self nonViolators.
	violators := self candidates reject: [ :candidate | nonViolators inject: false into: [ :bool :each | bool or: [ self same: each as: candidate ] ] ].
	^ violators
]
