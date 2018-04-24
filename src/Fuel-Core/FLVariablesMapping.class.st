"
I am used to materialize instance variables in an object, tolerating ""class shape changing"".

Cases tolerated are:
- instance variable added
- instance variable order change
- instance variable removed

"
Class {
	#name : #FLVariablesMapping,
	#superclass : #Object,
	#instVars : [
		'mapping',
		'theClass',
		'notIgnoredVariables',
		'references'
	],
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'instance creation' }
FLVariablesMapping class >> materializing: aClass from: aDecoder [
	
	^ self basicNew
		initializeWithClass: aClass;
		initializeMaterializingFrom: aDecoder;
		yourself.
]

{ #category : #'instance creation' }
FLVariablesMapping class >> newAnalyzing: anAnalysis references: aCollection [

	^ self basicNew
		initializeWithClass: anAnalysis references: aCollection;
		initializeAnalyzing;
		yourself
]

{ #category : #'serialize/materialize' }
FLVariablesMapping >> initializeAnalyzing [

	notIgnoredVariables := self instanceVariableNamesToSerialize.
	
	mapping := notIgnoredVariables 
		collect: [ :name | theClass instVarIndexFor: name ].
]

{ #category : #'serialize/materialize' }
FLVariablesMapping >> initializeMaterializingFrom: aDecoder [

	notIgnoredVariables := (1 to: aDecoder nextEncodedByte) collect: [ :index | aDecoder nextEncodedString ].

	mapping := notIgnoredVariables  
		collect: [ :name | theClass instVarIndexFor: name ifAbsent: [ nil ] ].


]

{ #category : #'initialize-release' }
FLVariablesMapping >> initializeWithClass: aClass [
	self initialize.
	theClass := aClass
]

{ #category : #'initialize-release' }
FLVariablesMapping >> initializeWithClass: aClass references: aCollection [
	self initialize.
	theClass := aClass.
	references := aCollection
]

{ #category : #private }
FLVariablesMapping >> instanceVariableNamesToSerialize [

	| ignoredInstanceVariableNames instanceVariableNamesToSerialize |
	ignoredInstanceVariableNames := theClass fuelIgnoredInstanceVariableNames.
	instanceVariableNamesToSerialize := OrderedCollection new.

	theClass instanceVariableNamesDo: [:name | 
		(ignoredInstanceVariableNames includes: name) 
			ifFalse: [ instanceVariableNamesToSerialize add: name ]].

	^ instanceVariableNamesToSerialize
]

{ #category : #accessing }
FLVariablesMapping >> map: sourceName to: destinationName [

	| indexOfVariableToMap |
	indexOfVariableToMap := notIgnoredVariables indexOf: sourceName.
	indexOfVariableToMap > 0
		ifTrue: [ mapping 
			at: indexOfVariableToMap 
			put: (theClass
				instVarIndexFor: destinationName
				ifAbsent: [self error: 'Bad variable destination.'])].

]

{ #category : #'serialize/materialize' }
FLVariablesMapping >> materializeReferencesOf: anObject with: aDecoder [ 

	mapping do: [ :index | 
			| reference |
			reference := aDecoder nextEncodedReference.
			index ifNotNil: [
				anObject 
					instVarAt: index
					put: reference ]].
]

{ #category : #accessing }
FLVariablesMapping >> references [
	^ references
]

{ #category : #analyzing }
FLVariablesMapping >> referencesOf: anObject do: aBlock [

	mapping do: [ :index | aBlock value: (anObject instVarAt: index) ].
]

{ #category : #'serialize/materialize' }
FLVariablesMapping >> serializeOn: anEncoder [

	anEncoder encodeByte: notIgnoredVariables size.	
	notIgnoredVariables do: [ :name | anEncoder encodeString: name ].
]

{ #category : #'serialize/materialize' }
FLVariablesMapping >> serializeReferencesOf: anObject with: anEncoder [ 

	(self references at: anObject ifAbsent: [ ^ self ]) do: [ :value |
		anEncoder encodeReferenceTo: value ].
]
