"
I am used to materialize instance variables in an object, tolerating ""class shape changing"".

Cases tolerated are:
- instance variable added
- instance variable order change
- instance variable removed

"
Class {
	#name : 'FLVariablesMapping',
	#superclass : 'Object',
	#instVars : [
		'mapping',
		'theClass',
		'notIgnoredVariables',
		'references',
		'possibleTraitVariables'
	],
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'instance creation' }
FLVariablesMapping class >> materializing: aClass from: aDecoder [
	
	^ self basicNew
		initializeWithClass: aClass;
		initializeMaterializingFrom: aDecoder;
		yourself.
]

{ #category : 'instance creation' }
FLVariablesMapping class >> newAnalyzing: anAnalysis references: aCollection [

	^ self basicNew
		initializeWithClass: anAnalysis references: aCollection;
		initializeAnalyzing;
		yourself
]

{ #category : 'serialize/materialize' }
FLVariablesMapping >> initializeAnalyzing [

	notIgnoredVariables := self instanceVariableNamesToSerialize.
	
	mapping := notIgnoredVariables 
		collect: [ :name | theClass instVarIndexFor: name ].
]

{ #category : 'serialize/materialize' }
FLVariablesMapping >> initializeMaterializingFrom: aDecoder [
	notIgnoredVariables := (1 to: aDecoder nextEncodedByte) collect: [ :index |
		aDecoder nextEncodedString ].

	mapping := notIgnoredVariables collect: [ :name |
		theClass
			instVarIndexFor: name
			ifAbsent: [ nil ] ].

	"Don't allocate unnecessary space for the dictionary if we don't need it."
	(mapping includes: nil) ifFalse: [ ^ self ].

	possibleTraitVariables := Dictionary new
]

{ #category : 'initialization' }
FLVariablesMapping >> initializeWithClass: aClass [
	self initialize.
	theClass := aClass
]

{ #category : 'initialization' }
FLVariablesMapping >> initializeWithClass: aClass references: aCollection [
	self initialize.
	theClass := aClass.
	references := aCollection
]

{ #category : 'private' }
FLVariablesMapping >> instanceVariableNamesToSerialize [

	| ignoredInstanceVariableNames instanceVariableNamesToSerialize |
	ignoredInstanceVariableNames := theClass fuelIgnoredInstanceVariableNames.
	instanceVariableNamesToSerialize := OrderedCollection new.

	theClass instanceVariableNamesDo: [:name | 
		(ignoredInstanceVariableNames includes: name) 
			ifFalse: [ instanceVariableNamesToSerialize add: name ]].

	^ instanceVariableNamesToSerialize
]

{ #category : 'accessing' }
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

{ #category : 'serialize/materialize' }
FLVariablesMapping >> materializeReferencesOf: anObject with: aDecoder [
	mapping withIndexDo: [ :varIndex :index |
		| reference |
		reference := aDecoder nextEncodedReference.
		varIndex
			ifNotNil: [
				anObject
					instVarAt: varIndex
					put: reference ]
			ifNil: [
				"Since the variables from traits will only become known
				after the references have been fully materialized we
				don't yet know whether this variable is to be ignored
				or might be needed later"
				possibleTraitVariables
					at: (notIgnoredVariables at: index)
					put: reference ] ]
]

{ #category : 'serialize/materialize' }
FLVariablesMapping >> materializeTraitVariablesWith: anObject [
	"If there are any trait variables, store their values.
	Only at this point do we know that we have trait variables
	because the trait composition is a reference that must be
	fully loaded first.
	
	See FLBehaviorCluster>>installLayout:withFormat:variableNames:into:
	and ClassDescription>>fuelRecomputeSlotScopeWithTraits."

	possibleTraitVariables ifNil: [ ^ self ].

	anObject class traitComposition slots do: [ :slot |
		| index |
		index := anObject class instVarIndexFor: slot name.
		index > 0 ifTrue: [
			anObject
				instVarAt: index
				put: (possibleTraitVariables
					at: slot name
					ifAbsent: nil) ] ]
]

{ #category : 'accessing' }
FLVariablesMapping >> references [
	^ references
]

{ #category : 'analyzing' }
FLVariablesMapping >> referencesOf: anObject do: aBlock [

	mapping do: [ :index | aBlock value: (anObject instVarAt: index) ].
]

{ #category : 'serialize/materialize' }
FLVariablesMapping >> serializeOn: anEncoder [

	anEncoder encodeByte: notIgnoredVariables size.	
	notIgnoredVariables do: [ :name | anEncoder encodeString: name ].
]

{ #category : 'serialize/materialize' }
FLVariablesMapping >> serializeReferencesOf: anObject with: anEncoder [ 

	(self references at: anObject ifAbsent: [ ^ self ]) do: [ :value |
		anEncoder encodeReferenceTo: value ].
]
