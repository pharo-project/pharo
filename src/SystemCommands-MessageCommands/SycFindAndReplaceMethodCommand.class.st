"
I am a command to find and replace selector in a range of methods, with a call to itself
"
Class {
	#name : #SycFindAndReplaceMethodCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #accessing }
SycFindAndReplaceMethodCommand class >> defaultMenuIconName [
	^#smallFindIcon
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> applyResultInContext: aToolContext [
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> defaultMenuItemName [
	^'Find and replace'
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> execute [
	|dialog refactoring|
	refactoring :=  RBFindAndReplaceMethodRefactoring 
		find: originalMessage contextUser.
	refactoring setOption: #listToSearch toUse: [ :met :ref |
		dialog := RBFindAndReplacePreview method: met refactoring: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ].
		ref].
	refactoring primitiveExecute 
]

{ #category : #testing }
SycFindAndReplaceMethodCommand >> isComplexRefactoring [
	^false
]

{ #category : #execution }
SycFindAndReplaceMethodCommand >> resultMessageSelector [
	^ nil
]
