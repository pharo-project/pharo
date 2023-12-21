"
This command find the senders of a given method in the classes upper in the inheritance chain above a given method.
"
Class {
	#name : 'SycShowLocalSendersCommand',
	#superclass : 'SycMessageCommand',
	#instVars : [
		'selectedClass',
		'selectedMessage'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycShowLocalSendersCommand >> applyResultInContext: a [
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> defaultMenuIconName [

	^ #edit
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> defaultMenuItemName [

	^ 'Hierarchical senders'
]

{ #category : 'execution' }
SycShowLocalSendersCommand >> execute [

	Smalltalk tools messageList browse: self searchInTheWholeHierarchy.
]

{ #category : 'testing' }
SycShowLocalSendersCommand >> isComplexRefactoring [

	^ false
]

{ #category : 'execution' }
SycShowLocalSendersCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	selectedMessage := aToolContext lastSelectedMessage.
	selectedClass := aToolContext lastSelectedClass.
]

{ #category : 'execution' }
SycShowLocalSendersCommand >> searchInTheWholeHierarchy [

	^ selectedClass withAllSuperAndSubclasses 
		flatCollect: [ :class |
			(class thoroughWhichMethodsReferTo: selectedMessage selector) , (class class thoroughWhichMethodsReferTo: selectedMessage selector) ].
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> selectedClass [

	^ selectedClass
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> selectedClass: anObject [

	selectedClass := anObject
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> selectedMessage [

	^ selectedMessage
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> selectedMessage: anObject [

	selectedMessage := anObject
]
