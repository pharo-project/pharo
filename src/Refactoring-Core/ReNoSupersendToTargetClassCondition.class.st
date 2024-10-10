"
I am a refactoring condition that checks if a method, defined in `class` (the source of a pulled method), is doing a supercall to a method defined in `targetSuperclass`. 
For clarity, `class` is one of `targetSuperclass` subclasses.

```
A >> foo ^ 12

   B >> xxx ^ super foo + 23 
```

Here the condition does not hold when class is B because B >> xxx is doing a supercall and moving it up would change the behavior of xxx.
"
Class {
	#name : 'ReNoSupersendToTargetClassCondition',
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
ReNoSupersendToTargetClassCondition >> class: rbClassA targetSuperclass: rbClassB selectors: aCollection [ 

	class := rbClassA.
	targetSuperclass := rbClassB.
	selectors := aCollection 
]

{ #category : 'displaying' }
ReNoSupersendToTargetClassCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator selector;
			nextPutAll:
				' sends a message to ''super'' which is resolved to target superclass. Pulling this method up to the target superclass would change the method lookup for the super send.';
			space ]
]

{ #category : 'accessing' }
ReNoSupersendToTargetClassCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	
	violators := OrderedCollection new.
	selectors do: [ :each |
		| parseTree |
		parseTree := class parseTreeForSelector: each.
		parseTree superMessages
			detect: [ :sup | targetSuperclass directlyDefinesMethod: sup ]
			ifFound: [ violators add: (class methodFor: each) ] ].
	^ violators
]
