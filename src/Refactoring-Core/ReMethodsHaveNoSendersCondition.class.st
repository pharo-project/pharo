Class {
	#name : 'ReMethodsHaveNoSendersCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'classSelectorsMapping',
		'model',
		'allSelectors'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReMethodsHaveNoSendersCondition >> allSelectors [

	^ allSelectors ifNil: [ allSelectors := classSelectorsMapping flatCollect: #value ]
]

{ #category : 'initialization' }
ReMethodsHaveNoSendersCondition >> classSelectorsMapping: aDictionary [

	classSelectorsMapping := aDictionary 
]

{ #category : 'initialization' }
ReMethodsHaveNoSendersCondition >> model: aRBNamespace [

	model := aRBNamespace 
]

{ #category : 'displaying' }
ReMethodsHaveNoSendersCondition >> violationMessageOn: aStream [

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
	self allSelectors do: [ :aSelector |
		model allReferencesTo: aSelector do: [ :aRBMethod |
			(self allSelectors includes: aRBMethod selector) ifFalse: [
				violators add: aSelector -> aRBMethod ] ] ].

	^ violators
]
