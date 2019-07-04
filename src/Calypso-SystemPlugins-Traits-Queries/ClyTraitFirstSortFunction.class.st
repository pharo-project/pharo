"
I sort classes in the order by name but I put traits before normal classes.
I am used in method visibility hierarchy where inherited traits are shown in merged hierarchy with user classes
"
Class {
	#name : #ClyTraitFirstSortFunction,
	#superclass : #ClySortFunction,
	#category : #'Calypso-SystemPlugins-Traits-Queries'
}

{ #category : #evaluating }
ClyTraitFirstSortFunction >> collate: aClass1 with: aClass2 [
	aClass1 isTrait & aClass2 isTrait not ifTrue: [ ^-1].
	aClass1 isTrait not & aClass2 isTrait ifTrue: [ ^1].
	
	^aClass1 name threeWayCompareTo: aClass2 name
]
