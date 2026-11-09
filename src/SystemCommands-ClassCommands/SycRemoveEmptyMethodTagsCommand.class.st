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
SycRemoveEmptyMethodTagsCommand >> executeRefactoring [

	classes do: [ :class | class removeEmptyProtocols ]
]

{ #category : 'accessing' }
SycRemoveEmptyMethodTagsCommand >> iconName [
	^#delete
]

{ #category : 'testing' }
SycRemoveEmptyMethodTagsCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserMethodGroupContext
]

{ #category : 'accessing' }
SycRemoveEmptyMethodTagsCommand >> name [
	^'Remove empty protocols'
]

{ #category : 'accessing' }
SycRemoveEmptyMethodTagsCommand >> order [

	^ 10001
]

{ #category : 'execution' }
SycRemoveEmptyMethodTagsCommand >> prepareFullExecution [

	super prepareFullExecution.
	classes := classes collect: [ :each | self context currentMetaLevelOf: each ]
]
