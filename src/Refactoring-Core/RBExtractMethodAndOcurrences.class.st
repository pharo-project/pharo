Class {
	#name : #RBExtractMethodAndOcurrences,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'extractionInterval',
		'selector',
		'extractedMethodSelector'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBExtractMethodAndOcurrences class >> extract: anInterval from: aSelector in: aClass [ 
	^self new
		extract: anInterval
		from: aSelector
		in: aClass
]

{ #category : #'instance creation' }
RBExtractMethodAndOcurrences class >> model: aRBSmalltalk extract: anInterval from: aSelector in: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		extract: anInterval
			from: aSelector
			in: aClass;
		yourself
]

{ #category : #'instance creation' }
RBExtractMethodAndOcurrences >> extract: anInterval from: aSelector in: aClass [ 
	class := self classObjectFor: aClass.
	selector := aSelector.
	extractionInterval := anInterval
]

{ #category : #transforming }
RBExtractMethodAndOcurrences >> extractMethod [
	| refactoring |
	refactoring := RBExtractMethodRefactoring 
				model: self model
				extract: extractionInterval
				from: selector
				in: class.
	self performComponentRefactoring: refactoring.
	^ refactoring newExtractedSelector
]

{ #category : #transforming }
RBExtractMethodAndOcurrences >> findImplementorOf: aSelector [
	(class directlyDefinesMethod: aSelector) ifTrue: [ ^ true ].
	class superclass ifNotNil: [
		class := class superclass.
		self findImplementorOf: aSelector.
	].
	^ false
]

{ #category : #transforming }
RBExtractMethodAndOcurrences >> findOcurrencesOf: aSelector [
	|refactoring|
	(self findImplementorOf: aSelector) ifTrue: [
		refactoring := RBFindAndReplaceRefactoring 
			model: self model 
			find: aSelector
			of: class
			inAllHierarchy: true.
		self performComponentRefactoring: refactoring.]
	ifFalse: [ self refactoringWarning: aSelector , ' is not a valid selector name.' ]
]

{ #category : #preconditions }
RBExtractMethodAndOcurrences >> preconditions [
	^RBCondition empty
]

{ #category : #'as yet unclassified' }
RBExtractMethodAndOcurrences >> storeOn: aStream [ 
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
RBExtractMethodAndOcurrences >> transform [ 
	| newExtractedSelector |
	newExtractedSelector := self extractMethod.
	self findOcurrencesOf: newExtractedSelector.
]
