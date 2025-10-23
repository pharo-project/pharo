"
I fail when a method is sent from any method in the model, except for those that are among my candidates.
"
Class {
	#name : 'ReMethodsHaveNoSendersCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'classSelectorsMapping',
		'model'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReMethodsHaveNoSendersCondition >> candidates [

	^ classSelectorsMapping flatCollect: [ :assoc | assoc value collect: [ :selector | assoc key -> (assoc key ifNotNil: [ :cls | cls methodFor: selector ] ifNil: [ nil ] ) ] ]
]

{ #category : 'initialization' }
ReMethodsHaveNoSendersCondition >> classSelectorsMapping: aDictionary [

	classSelectorsMapping := aDictionary.
	selectors := classSelectorsMapping flatCollect: #value
]

{ #category : 'initialization' }
ReMethodsHaveNoSendersCondition >> model: aRBNamespace [

	model := aRBNamespace 
]

{ #category : 'displaying' }
ReMethodsHaveNoSendersCondition >> violationMessageOn: aStream [
	self violators ifEmpty: [ ^ self ].
	
	aStream nextPutAll: 'Method(s) have senders:'.
	self violators do: [ :violator |
		aStream
			nextPutAll: violator asString;
			nextPutAll: ';';
			space ]
]

{ #category : 'accessing' }
ReMethodsHaveNoSendersCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := OrderedCollection new.
	self candidates do: [ :aSelector | | externalSenders |
			model allReferencesTo: aSelector do: [ :aRBMethod |
					externalSenders := classSelectorsMapping select: [ :assoc |
						 assoc key = aRBMethod methodClass and: [ assoc value includes: aRBMethod selector ] ].
					externalSenders ifEmpty: [
						violators add: aSelector -> aRBMethod ] ] ].

	^ violators
]
