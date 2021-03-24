Class {
	#name : #RBExtractSetUpMethodAndOccurrences,
	#superclass : #RBExtractMethodAndOccurrences,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBExtractSetUpMethodAndOccurrences >> extractMethodClass [
	^ RBExtractSetUpMethodRefactoring
]

{ #category : #transforming }
RBExtractSetUpMethodAndOccurrences >> findOccurrencesClass [
	^ RBFindAndReplaceSetUpRefactoring
]

{ #category : #transforming }
RBExtractSetUpMethodAndOccurrences >> transform [ 
	self extractMethod.
	self findOccurrencesOf: #setUp
]
