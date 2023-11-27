Class {
	#name : 'RBMethodsHaveNoSendersCondition',
	#superclass : 'RBMethodsCondition',
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
RBMethodsHaveNoSendersCondition >> allSelectors [

	^ allSelectors ifNil: [ allSelectors := classSelectorsMapping flatCollect: #value ]
]

{ #category : 'initialization' }
RBMethodsHaveNoSendersCondition >> classSelectorsMapping: aDictionary [

	classSelectorsMapping := aDictionary 
]

{ #category : 'initialization' }
RBMethodsHaveNoSendersCondition >> model: aRBNamespace [

	model := aRBNamespace 
]

{ #category : 'displaying' }
RBMethodsHaveNoSendersCondition >> violationMessageOn: aStream [

	aStream nextPutAll: 'Method(s) have senders:'.
	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ';';
			space ]
]

{ #category : 'accessing' }
RBMethodsHaveNoSendersCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := OrderedCollection new.
	self allSelectors do: [ :aSelector |
		model allReferencesTo: aSelector do: [ :aRBMethod |
			(self allSelectors includes: aRBMethod selector) ifFalse: [
				violators add: aSelector -> aRBMethod ] ] ].

	^ violators
]
