"
I am a command to find and replace selector in a range of methods, with a call to itself
"
Class {
	#name : 'SycFindAndReplaceMethodCommand',
	#superclass : 'SycMessageCmCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycFindAndReplaceMethodCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : 'execution' }
SycFindAndReplaceMethodCommand >> applyCommandResult [
]

{ #category : 'execution' }
SycFindAndReplaceMethodCommand >> createRefactoring [

	^ RBFindAndReplaceTransformation
		model: model
		find: originalMessage selector
		of: originalMessage contextUser origin
		inWholeHierarchy: self searchInTheWholeHierarchy
]

{ #category : 'accessing' }
SycFindAndReplaceMethodCommand >> icon [

	^ self iconNamed: #smallFind
]

{ #category : 'accessing' }
SycFindAndReplaceMethodCommand >> name [

	^ self refactoringClass menuItemString
]

{ #category : 'accessing' }
SycFindAndReplaceMethodCommand >> order [

	^ 25
]

{ #category : 'factory method' }
SycFindAndReplaceMethodCommand >> refactoringClass [

	^ RBFindAndReplaceTransformation
]

{ #category : 'execution' }
SycFindAndReplaceMethodCommand >> resultMessageSelector [
	^ nil
]

{ #category : 'testing' }
SycFindAndReplaceMethodCommand >> searchInTheWholeHierarchy [

	^ originalMessage contextUser origin subclasses
		ifEmpty: [ false ]
		ifNotEmpty: [ self confirm: 'Do you want to search occurrences in the whole hierarchy?' ]
]
