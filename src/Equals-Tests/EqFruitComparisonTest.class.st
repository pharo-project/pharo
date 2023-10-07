Class {
	#name : 'EqFruitComparisonTest',
	#superclass : 'TestCase',
	#category : 'Equals-Tests',
	#package : 'Equals-Tests'
}

{ #category : 'tests' }
EqFruitComparisonTest >> testComparisonfOfApplesOfDifferentColorsShouldNotBeEquals [
	| apple1 apple2 |
	
	apple1 := EqApple color: Color red.
	apple2 := EqApple color: Color yellow.
	
	self deny: apple1 equals: apple2
]

{ #category : 'tests' }
EqFruitComparisonTest >> testComparisonfOfApplesOfSameColorAtDifferentStagesShouldNotBeEquals [
	| apple1 apple2 |
	
	apple1 := EqApple color: Color red.
	apple1 beRipe.
	apple2 := EqApple color: Color yellow.
	apple2 beRotten.	
		
	self deny: apple1 equals: apple2
]

{ #category : 'tests' }
EqFruitComparisonTest >> testComparisonfOfApplesOfSameColorShouldBeEquals [
	| apple1 apple2 |
	
	apple1 := EqApple color: Color red.
	apple2 := EqApple color: Color red.
	
	self assert: apple1 equals: apple2
]

{ #category : 'tests' }
EqFruitComparisonTest >> testComparisonfOfYellowAppleAndBananaShouldNotBeEquals [
	| apple banana |
	
	apple := EqApple color: Color yellow.
	banana := EqBanana new.
	
	self deny: apple equals: banana
]
