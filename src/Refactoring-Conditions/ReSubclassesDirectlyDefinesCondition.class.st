"
I'm a condition that checks if any subclass of given class redefines any given selector
"
Class {
	#name : 'ReSubclassesDirectlyDefinesCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'initialization' }
ReSubclassesDirectlyDefinesCondition >> class: aRBClass selectors: aCollection [

	class := aRBClass.
	selectors := aCollection
]

{ #category : 'displaying' }
ReSubclassesDirectlyDefinesCondition >> violationMessageOn: aStream [

	self violators keysAndValuesDo: [ :cls :methods |
			aStream
				nextPutAll: 'Subclass of ';
				nextPutAll: class name;
				nextPutAll: ' (';
				nextPutAll: cls name;
				nextPutAll: ') overrides ';
				nextPutAll: (', ' join: (methods collect: #selector));
				nextPut: $.;
				cr ]
]

{ #category : 'accessing' }
ReSubclassesDirectlyDefinesCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	violators := Dictionary new.

	selectors do: [ :selector |
			class subclasses do: [ :subclass |
					(subclass selectors includes: selector) ifTrue: [
							violators
								at: subclass
								ifPresent: [ :methods | methods add: (subclass methodFor: selector) ]
								ifAbsentPut: [ OrderedCollection with: (subclass methodFor: selector) ] ] ] ].

	^ violators
]
