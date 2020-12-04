Class {
	#name : #CamelCaseSplitTest,
	#superclass : #TestCase,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
CamelCaseSplitTest >> testSingleLowercaseLetter [

	self 
		assertCollection: 'a' splitCamelCase 
		hasSameElements: {'a'}
]

{ #category : #tests }
CamelCaseSplitTest >> testSingleUppercaseLetter [

	self 
		assertCollection: 'A' splitCamelCase 
		hasSameElements: { 'A' }
]

{ #category : #tests }
CamelCaseSplitTest >> testSplittingShouldNotLosePrefix [

	self 
		assertCollection: 'GTSpotter' splitCamelCase 
		hasSameElements: {'GTSpotter'}
]

{ #category : #tests }
CamelCaseSplitTest >> testTwoDifferentWordsStartingWithLowerCase [

	self 
		assertCollection: 'anObject' splitCamelCase 
		hasSameElements: {'an'. 'Object'}
]

{ #category : #tests }
CamelCaseSplitTest >> testTwoDifferentWordsStartingWithUpperCase [

	self 
		assertCollection: 'AnObject' splitCamelCase 
		hasSameElements: {'An'. 'Object'}
]
