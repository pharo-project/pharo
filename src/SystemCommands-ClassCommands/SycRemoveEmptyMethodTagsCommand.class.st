"
I am a command to clean all unused method tags (empty protocols) of given classes
"
Class {
	#name : 'SycRemoveEmptyMethodTagsCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycRemoveEmptyMethodTagsCommand >> execute [

	classes do: [ :class | class removeEmptyProtocols ]
]

{ #category : 'accessing' }
SycRemoveEmptyMethodTagsCommand >> iconName [
	^#delete
]

{ #category : 'accessing' }
SycRemoveEmptyMethodTagsCommand >> name [
	^'Remove empty protocols'
]

{ #category : 'execution' }
SycRemoveEmptyMethodTagsCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each]
]
