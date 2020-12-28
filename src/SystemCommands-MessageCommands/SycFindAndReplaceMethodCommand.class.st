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

{ #category : #accessing }
SycFindAndReplaceMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 25 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> applyResultInContext: aToolContext [
]

{ #category : #testing }
SycFindAndReplaceMethodCommand >> createRefactoring [
	^RBFindAndReplaceRefactoring 
		find: originalMessage selector 
		of: originalMessage contextUser origin
		inAllHierarchy: true
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> defaultMenuItemName [
	^'Find and replace'
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> execute [
	self executeRefactorings 
]

{ #category : #testing }
SycFindAndReplaceMethodCommand >> isComplexRefactoring [
	^false
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> resultMessageSelector [
	^ nil
]
