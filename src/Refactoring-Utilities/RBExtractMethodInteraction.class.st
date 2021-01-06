Class {
	#name : #RBExtractMethodInteraction,
	#superclass : #RBSourceCodeInteraction,
	#instVars : [
		'selectedTextInterval',
		'refactoring'
	],
	#category : #'Refactoring-Utilities'
}

{ #category : #accessing }
RBExtractMethodInteraction >> asRefactorings [
	refactoring := RBExtractMethodRefactoring
		extract: self selectedTextInterval
	   from: method selector
	   in: method origin.
	^ { refactoring }
]

{ #category : #actions }
RBExtractMethodInteraction >> executeRefactoring [ 
	super executeRefactoring.
	self findOccurrencesInWholeClass 
		ifTrue: [ 
			self findOccurrencesOf: refactoring newExtractedSelector
		]
]

{ #category : #actions }
RBExtractMethodInteraction >> findOccurrencesInAllHierarchy [
	^ self confirm: 'Do you want find occurrences in all',
		 String cr, ' class hierarchy?'
]

{ #category : #actions }
RBExtractMethodInteraction >> findOccurrencesInWholeClass [
	^ self confirm: 'Do you want to search for the occurrences',
		 String cr, ' of the extracted method in the whole class?'
]

{ #category : #actions }
RBExtractMethodInteraction >> findOccurrencesOf: aSelector [
	refactoring := RBFindAndReplaceRefactoring
			find: aSelector
			of: method origin
			inAllHierarchy: self findOccurrencesInAllHierarchy.
	refactoring execute
]

{ #category : #accessing }
RBExtractMethodInteraction >> prepareFullExecution [
	super prepareFullExecution.
	self selectedTextInterval
]

{ #category : #accessing }
RBExtractMethodInteraction >> selectedTextInterval [
	^ selectedTextInterval ifNil: [ selectedTextInterval := context selectedTextInterval ]
]
