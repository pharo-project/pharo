"
I implement sorting of different kind of system items like methods and classes.

First I check the type of given items using #calypsoEnvironmentType. If items are belongs to same type I use their #defaultSortFunctionForCalypso to sort them.
In addition I cache all created sort functions.

And when items are belongs to the different type I compare this types using #itemsSortOrderForCalypso number.
Look at #collate:with: method for details
 
Internal Representation and Key Implementation Points.

    Instance Variables
	functionsForTypes:		<Dictionary<Class, SortFunction>>
"
Class {
	#name : #ClySortSystemItemFunction,
	#superclass : #ClySortFunction,
	#instVars : [
		'functionsForTypes'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #evaluating }
ClySortSystemItemFunction >> collate: systemObject1 with: systemObject2 [

	| function |
	systemObject1 calypsoEnvironmentType = systemObject2 calypsoEnvironmentType ifTrue: [ 
		function := self sortFunctionFor: systemObject1.
		^function collate: systemObject1 with: systemObject2 ].
	
	^systemObject1 calypsoEnvironmentType itemsSortOrderForCalypso
		threeWayCompareTo: systemObject2 calypsoEnvironmentType itemsSortOrderForCalypso
]

{ #category : #initialization }
ClySortSystemItemFunction >> initialize [
	super initialize.
	
	functionsForTypes := IdentityDictionary new.
]

{ #category : #evaluating }
ClySortSystemItemFunction >> sortFunctionFor: systemObject [

	^functionsForTypes 
		at: systemObject calypsoEnvironmentType 
		ifAbsentPut: [ systemObject calypsoEnvironmentType defaultSortFunctionForCalypso].
		
]
