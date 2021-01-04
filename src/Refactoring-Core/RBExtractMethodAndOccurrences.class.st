Class {
	#name : #RBExtractMethodAndOccurrences,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'extractionInterval',
		'selector',
		'extractedMethodSelector'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBExtractMethodAndOccurrences class >> extract: anInterval from: aSelector in: aClass [ 
	^self new
		extract: anInterval
		from: aSelector
		in: aClass
]

{ #category : #'instance creation' }
RBExtractMethodAndOccurrences class >> model: aRBSmalltalk extract: anInterval from: aSelector in: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		extract: anInterval
			from: aSelector
			in: aClass;
		yourself
]

{ #category : #'instance creation' }
RBExtractMethodAndOccurrences >> extract: anInterval from: aSelector in: aClass [ 
	class := self classObjectFor: aClass.
	selector := aSelector.
	extractionInterval := anInterval
]

{ #category : #transforming }
RBExtractMethodAndOccurrences >> extractMethod [
	| refactoring |
	refactoring := RBExtractMethodRefactoring 
				model: self model
				extract: extractionInterval
				from: selector
				in: class.
	self performCompositeRefactoring: refactoring.
	^ refactoring newExtractedSelector
]

{ #category : #transforming }
RBExtractMethodAndOccurrences >> findImplementorOf: aSelector [
	(class directlyDefinesMethod: aSelector) ifTrue: [ ^ true ].
	class superclass ifNotNil: [
		class := class superclass.
		self findImplementorOf: aSelector.
	].
	^ false
]

{ #category : #transforming }
RBExtractMethodAndOccurrences >> findOccurrencesOf: aSelector [
	|refactoring|
	(self findImplementorOf: aSelector) ifTrue: [
		refactoring := RBFindAndReplaceRefactoring 
			model: self model 
			find: aSelector
			of: class
			inAllHierarchy: true.
		self performCompositeRefactoring: refactoring.]
	ifFalse: [ self refactoringWarning: aSelector , ' is not a valid selector name.' ]
]

{ #category : #preconditions }
RBExtractMethodAndOccurrences >> preconditions [

	^ self emptyCondition
]

{ #category : #printing }
RBExtractMethodAndOccurrences >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' extract: '.
	extractionInterval storeOn: aStream.
	aStream
		nextPutAll: ' from: #';
		nextPutAll: selector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBExtractMethodAndOccurrences >> transform [ 
	| newExtractedSelector |
	newExtractedSelector := self extractMethod.
	self findOccurrencesOf: newExtractedSelector.
]
