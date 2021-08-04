"
I am a command to find and replace selector in a range of methods, with a call to itself
"
Class {
	#name : #SycFindAndReplaceMethodCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #testing }
SycFindAndReplaceMethodCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : #accessing }
SycFindAndReplaceMethodCommand class >> defaultMenuIconName [
	^#smallFindIcon
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> applyResultInContext: aToolContext [
]

{ #category : #testing }
SycFindAndReplaceMethodCommand >> createRefactoring [

	^ RBFindAndReplaceRefactoring
		model: model
		find: originalMessage selector
		of: originalMessage contextUser origin
		inWholeHierarchy: self searchInTheWholeHierarchy
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> defaultMenuItemName [
	^'Find and replace'
]

{ #category : #testing }
SycFindAndReplaceMethodCommand >> isComplexRefactoring [
	^ true
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> resultMessageSelector [
	^ nil
]

{ #category : #testing }
SycFindAndReplaceMethodCommand >> searchInTheWholeHierarchy [

	^ originalMessage contextUser origin subclasses 
		ifEmpty: [ false ]
		ifNotEmpty: [ self confirm: 'Do you want to search occurrences in the whole hierarchy?' ]
]
