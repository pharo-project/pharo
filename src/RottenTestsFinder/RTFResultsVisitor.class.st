Class {
	#name : 'RTFResultsVisitor',
	#superclass : 'Object',
	#category : 'RottenTestsFinder-Visitors',
	#package : 'RottenTestsFinder',
	#tag : 'Visitors'
}

{ #category : 'visiting' }
RTFResultsVisitor >> visit: anObject [
	^ anObject acceptVisitor: self
]

{ #category : 'visiting' }
RTFResultsVisitor >> visitRottenTest: aRottenTest [
	^ self subclassResponsibility
]

{ #category : 'visiting' }
RTFResultsVisitor >> visitRottenTestHelper: aRottenTestHelper [
	^ self subclassResponsibility
]

{ #category : 'visiting' }
RTFResultsVisitor >> visitRottenTestImplementedInSuperClass: aRottenTestImplementedInSuperClass [
	^ self subclassResponsibility
]

{ #category : 'visiting' }
RTFResultsVisitor >> visitRottenTestImplementedInTrait: aRottenTestImplementedInTrait [
	^ self subclassResponsibility
]

{ #category : 'visiting' }
RTFResultsVisitor >> visitRottenTestsSet: aRottenTestsSet [
	^ self subclassResponsibility
]
