"
I check if every single subclass of given class defines one of given selectors
"
Class {
	#name : 'ReAllSubclassesDirectlyDefinesCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'initialization' }
ReAllSubclassesDirectlyDefinesCondition >> class: aRBClass selectors: aCollection [

	class := aRBClass.
	selectors := aCollection
]

{ #category : 'displaying' }
ReAllSubclassesDirectlyDefinesCondition >> violationMessageOn: aStream [

	| selectorsForClass |
	selectorsForClass := Set new.

	self violators valuesDo: [ :methods | selectorsForClass addAll: (methods collect: [ :method | method selector ]) ].

	selectorsForClass ifNotEmpty: [
			aStream
				nextPutAll: 'All subclasses of ';
				nextPutAll: class name;
				nextPutAll: ' override ';
				nextPutAll: (', ' join: (selectorsForClass collect: #asString));
				nextPut: $.;
				cr ]
]

{ #category : 'accessing' }
ReAllSubclassesDirectlyDefinesCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	violators := Dictionary new.
	selectors do: [ :selector |
			class subclasses ifNotEmpty: [
					(class subclasses allSatisfy: [ :subclass | subclass selectors includes: selector ]) ifTrue: [
							class subclasses do: [ :subclass |
									violators
										at: subclass
										ifPresent: [ :methods | methods add: (subclass methodFor: selector) ]
										ifAbsentPut: [ OrderedCollection with: (subclass methodFor: selector) ] ] ] ] ].
	^ violators
]
