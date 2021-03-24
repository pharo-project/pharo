Class {
	#name : #RBExtractMethodSetUpAndOccurrences,
	#superclass : #RBExtractMethodAndOccurrences,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBExtractMethodSetUpAndOccurrences >> extractMethodClass [
	^ RBExtractSetUpMethodRefactoring
]

{ #category : #transforming }
RBExtractMethodSetUpAndOccurrences >> findOccurrencesClass [
	^ RBFindAndReplaceSetUpRefactoring
]
