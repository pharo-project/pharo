"
I wrap a generator to provide the same API than the iterators.
My subclasses should implement #elementsDo: to iterate the collection or implement the values.
In this method, each of the generated values should be used with the valuable pased.


Ex:
elementsDo: aValuable
	[ true ] whileTrue: [ aValuable value: 42  ].
	
This example generates an infinite iterator always returning 42.
Using the generator will produce that the method is only executed on demand.

This Iterator can also wrap collections, iterating one element at the time.

Ex:
elementsDo: aValuable
	SystemNavigation default allBehaviorsDo: aValuable
"
Class {
	#name : #SptGenericGenerator,
	#superclass : #SptIterator,
	#instVars : [
		'generator'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #testing }
SptGenericGenerator >> atEnd [ 

	^ generator atEnd
]

{ #category : #protected }
SptGenericGenerator >> doReset [
	generator := Generator on: [ :aGenerator | 
		self elementsDo: [ :each | aGenerator yield: each ] ]
]

{ #category : #enumerating }
SptGenericGenerator >> elementsDo: aValuable [

	^ self subclassResponsibility
]

{ #category : #initialization }
SptGenericGenerator >> initialize [
	super initialize.
	self reset
]

{ #category : #accessing }
SptGenericGenerator >> next [

	^ generator next
]

{ #category : #initialization }
SptGenericGenerator >> next: aQuantity [

	^ generator next: aQuantity
]
