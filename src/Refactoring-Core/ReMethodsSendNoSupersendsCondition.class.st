"
I am a refactoring condition that checks if a method, defined in `class`, is doing a supercall to a method defined in `targetSuperclass`. For clarity, `class` is one of `targetSuperclass` subclasses
"
Class {
	#name : 'ReMethodsSendNoSupersendsCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors',
		'targetSuperclass'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReMethodsSendNoSupersendsCondition >> class: rbClassA targetSuperclass: rbClassB selectors: aCollection [ 

	class := rbClassA.
	targetSuperclass := rbClassB.
	selectors := aCollection 
]

{ #category : 'accessing' }
ReMethodsSendNoSupersendsCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	
	violators := OrderedCollection new.
	selectors do: [ :each |
		| parseTree |
		parseTree := class parseTreeForSelector: each.
		parseTree superMessages
			detect: [ :sup | targetSuperclass directlyDefinesMethod: sup ]
			ifFound: [ violators add: each ] ].
	^ violators
]
