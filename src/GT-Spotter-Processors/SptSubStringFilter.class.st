"
I implement a simple substring string filter using includesSubstring:.
"
Class {
	#name : #SptSubStringFilter,
	#superclass : #SptAbstractStringFilter,
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #protected }
SptSubStringFilter >> criterium: aValue [

	^ aValue asString asLowercase includesSubstring: filteringText asLowercase


]

{ #category : #filtering }
SptSubStringFilter >> substringFilter: aString [
	
	self filteringText: aString
]
