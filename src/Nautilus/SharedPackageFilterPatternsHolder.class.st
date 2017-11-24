"
A SharedPackageFilterPatternsHolder is used for package filter input. It shares historical values of the input among all the PackageTreeNautilus browsers. Variable lastValue keeps the last used value. It is used when a browser is opened from a system (e.g. World menu, anObject browse).

Instance Variables
	mutex:		Mutex
	value:		OrderedCollection
	lastValue:	String

"
Class {
	#name : #SharedPackageFilterPatternsHolder,
	#superclass : #Object,
	#instVars : [
		'value',
		'mutex',
		'lastValue'
	],
	#classInstVars : [
		'instance'
	],
	#category : #'Nautilus-Tree'
}

{ #category : #cleanup }
SharedPackageFilterPatternsHolder class >> cleanUp [
	self reset
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder class >> instance [

	^ instance ifNil: [ instance := self new value: OrderedCollection new; yourself ]
]

{ #category : #cleanup }
SharedPackageFilterPatternsHolder class >> reset [
	instance := nil
]

{ #category : #adding }
SharedPackageFilterPatternsHolder >> addFirst: aString [ 
	^ mutex critical: [ 
		value 
			detect: [ :eachPattern | aString beginsWith: eachPattern ]
			ifFound: [ :aPattern | value remove: aPattern ].
		value addFirst: aString.
	 ]
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> at: index [

	^ mutex critical: [ value at: index ]
]

{ #category : #enumerating }
SharedPackageFilterPatternsHolder >> collectWithIndex: aBlockClosure [ 
	
	^ mutex critical: [ value withIndexCollect: aBlockClosure ]
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> firstOrNil [
	^ mutex critical: [ 
		value 
			ifNotEmpty: [ value first ] 
			ifEmpty: [ nil ] ]
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> indexOf: aString [ 
	
	^ mutex critical: [ value indexOf: aString ]
]

{ #category : #initialization }
SharedPackageFilterPatternsHolder >> initialize [

	super initialize.
	mutex := Mutex new.
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> lastValue [
	^ mutex critical: [ lastValue ]
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> lastValue: aValue [ 
	mutex critical: [ lastValue := aValue ]
]

{ #category : #removing }
SharedPackageFilterPatternsHolder >> remove: aString ifAbsent: aBlockClosure [ 
	
	^ mutex critical: [ value remove: aString ifAbsent: aBlockClosure ]
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> size [

	^ mutex critical: [ value size ]
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> value [

	^ mutex critical: [ value ].
]

{ #category : #accessing }
SharedPackageFilterPatternsHolder >> value: anObject [

	mutex critical: [ value := anObject ].
]
