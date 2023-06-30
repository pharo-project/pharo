Class {
	#name : #SycRenameMessage2Command,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSignature'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #requesting }
SycRenameMessage2Command >> canAddArgs [
	^false
]

{ #category : #requesting }
SycRenameMessage2Command >> canRemoveArgs [
	^ false
]

{ #category : #execution }
SycRenameMessage2Command >> createRefactoring [

	^ RBRenameMethodDriver new
			model: model
			renameMethodSignature: originalMessage 
			in:  refactoredClass 

		
]

{ #category : #accessing }
SycRenameMessage2Command >> defaultMenuIconName [
	^#edit
]

{ #category : #accessing }
SycRenameMessage2Command >> defaultMenuItemName [
	^'Rename2'
]

{ #category : #execution }
SycRenameMessage2Command >> isComplexRefactoring [ 

	^ false
]

{ #category : #accessing }
SycRenameMessage2Command >> newSignature [
	^ newSignature
]

{ #category : #accessing }
SycRenameMessage2Command >> newSignature: anObject [
	newSignature := anObject
]

{ #category : #execution }
SycRenameMessage2Command >> resultMessageSelector [
	^newSignature selector
]
