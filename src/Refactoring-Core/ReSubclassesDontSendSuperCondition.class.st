Class {
	#name : 'ReSubclassesDontSendSuperCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReSubclassesDontSendSuperCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection 
]

{ #category : 'testing' }
ReSubclassesDontSendSuperCondition >> isFalse [

	^ self check not
]

{ #category : 'displaying' }
ReSubclassesDontSendSuperCondition >> violationMessageOn: aStream [

	aStream nextPutAll: 'Subclass of '.

	self violators keysAndValuesDo: [ :cls :sels |
		aStream
			nextPutAll: cls name;
			nextPutAll: ' is doing a super send using ';
			nextPutAll: (', ' join: sels);
			nextPut: $.;
			cr.
	 ]
]

{ #category : 'accessing' }
ReSubclassesDontSendSuperCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	violators := Dictionary new.

	class subclasses do: [ :each |
		selectors do: [ :aSelector |
			each selectors
				detect: [ :sel |
					| method tree |
					method := each methodFor: sel.
					tree := each parseTreeForSelector: sel.
					tree isNotNil and: [ tree superMessages includes: aSelector ] ]
				ifFound: [ :sel |
					| method |
					method := each methodFor: sel.
					violators
						at: each
						ifPresent: [ (violators at: each) add: method ]
						ifAbsentPut: [ OrderedCollection with: method ] ] ] ].
	^ violators
]
