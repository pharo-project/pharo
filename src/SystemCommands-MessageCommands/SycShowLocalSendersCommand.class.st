"
This command find the senders of a given method in the classes upper in the inheritance chain above a given method.
"
Class {
	#name : 'SycShowLocalSendersCommand',
	#superclass : 'SycMessageCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycShowLocalSendersCommand class >> canBeExecutedInContext: aToolContext [

	^ (super canBeExecutedInContext: aToolContext) and: [ aToolContext selectedMessages size = 1 ]
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> defaultMenuIconName [

	^ #edit
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> defaultMenuItemName [

	^ 'Hierarchical senders'
]

{ #category : 'accessing' }
SycShowLocalSendersCommand >> description [

	^ 'Finds Hierarchical senders of a method'
]

{ #category : 'execution' }
SycShowLocalSendersCommand >> execute [

	(Smalltalk tools toolNamed: #messageList) browse: self searchInTheWholeHierarchy.
]

{ #category : 'execution' }
SycShowLocalSendersCommand >> searchInTheWholeHierarchy [

	^ self selectedClass withAllSuperAndSubclasses 
		flatCollect: [ :class |
			(class thoroughWhichMethodsReferTo: self selectedSelector) , (class class thoroughWhichMethodsReferTo: self selectedSelector) ].
]
