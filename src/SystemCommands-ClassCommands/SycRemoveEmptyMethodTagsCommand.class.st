"
I am a command to clean all unused method tags (empty protocols) of given classes
"
Class {
	#name : #SycRemoveEmptyMethodTagsCommand,
	#superclass : #SycClassCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #accessing }
SycRemoveEmptyMethodTagsCommand >> defaultMenuIconName [
	^#delete
]

{ #category : #accessing }
SycRemoveEmptyMethodTagsCommand >> defaultMenuItemName [
	^'Remove empty protocols'
]

{ #category : #execution }
SycRemoveEmptyMethodTagsCommand >> execute [
	
	classes do: [ :each | 
		each organization removeEmptyCategories]
]

{ #category : #execution }
SycRemoveEmptyMethodTagsCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each]
]
