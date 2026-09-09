"
I am a condition that checks if the superclass of my target class already defines one of my selectors
"
Class {
	#name : 'ReNoSuperclassDefinesCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors',
		'targetSuperclass'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'initialization' }
ReNoSuperclassDefinesCondition >> class: rbClassA targetSuperclass: rbClassB selectors: aCollection [

	class := rbClassA.
	targetSuperclass := rbClassB.
	selectors := aCollection
]

{ #category : 'displaying' }
ReNoSuperclassDefinesCondition >> violationMessageOn: aStream [

	self violators do: [ :pair |
			| definer selector |
			definer := pair first.
			selector := pair second.

			aStream
				nextPutAll: 'Superclass ';
				nextPutAll: definer name;
				nextPutAll: ' defines selector ';
				nextPutAll: selector;
				cr ]
]

{ #category : 'accessing' }
ReNoSuperclassDefinesCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := OrderedCollection new.

	selectors do: [ :each |
			| definer |
			definer := targetSuperclass whichClassIncludesSelector: each.
			definer ifNotNil: [
					violators add: {
							definer.
							each } ] ].

	^ violators
]
