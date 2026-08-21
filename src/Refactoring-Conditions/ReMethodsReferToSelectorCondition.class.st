Class {
	#name : 'ReMethodsReferToSelectorCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'initialization' }
ReMethodsReferToSelectorCondition >> class: aRBClass selectors: aCollection [

	class := aRBClass.
	selectors := aCollection
]

{ #category : 'displaying' }
ReMethodsReferToSelectorCondition >> violationMessageOn: aStream [

	self violators keysAndValuesDo: [ :selector :callers |
			aStream
				nextPutAll: 'Method ';
				nextPutAll: selector;
				nextPutAll: ' is called by ';
				nextPutAll: (', ' join: (callers collect: #selector));
				nextPutAll: ' in ';
				nextPutAll: class name;
				nextPut: $.;
				cr ]
]

{ #category : 'accessing' }
ReMethodsReferToSelectorCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	violators := Dictionary new.
	selectors do: [ :selector |
			class methods do: [ :caller |
					(caller refersToSymbol: selector) ifTrue: [
						violators at: selector ifPresent: [ :callers | callers add: caller ] ifAbsentPut: [ OrderedCollection with: caller ] ] ] ].
	^ violators
]
