Class {
	#name : #RBExtractMethodDriver,
	#superclass : #RBSourceCodeDriver,
	#instVars : [
		'selectedTextInterval',
		'refactoring'
	],
	#category : #'Refactoring-Utilities'
}

{ #category : #accessing }
RBExtractMethodDriver >> asRefactorings [
	refactoring := RBExtractMethodRefactoring
		extract: self selectedTextInterval
	   from: method selector
	   in: method origin.
	^ { refactoring }
]

{ #category : #actions }
RBExtractMethodDriver >> executeRefactoring [ 
	super executeRefactoring.
	self findOccurrencesInWholeClass 
		ifTrue: [ 
			self findOccurrencesOf: refactoring newExtractedSelector
		]
]

{ #category : #actions }
RBExtractMethodDriver >> findOccurrencesInAllHierarchy [
	^ self confirm: 'Do you want find occurrences in all',
		 String cr, ' class hierarchy?'
]

{ #category : #actions }
RBExtractMethodDriver >> findOccurrencesInWholeClass [
	^ self confirm: 'Do you want to search for the occurrences',
		 String cr, ' of the extracted method in the whole class?'
]

{ #category : #actions }
RBExtractMethodDriver >> findOccurrencesOf: aSelector [
	refactoring := RBFindAndReplaceRefactoring
			find: aSelector
			of: method origin
			inAllHierarchy: self findOccurrencesInAllHierarchy.
	refactoring execute
]

{ #category : #accessing }
RBExtractMethodDriver >> prepareFullExecution [
	super prepareFullExecution.
	self selectedTextInterval
]

{ #category : #accessing }
RBExtractMethodDriver >> selectedTextInterval [
	^ selectedTextInterval ifNil: [ selectedTextInterval := context selectedTextInterval ]
]
