"
This command find the senders of a given method in the classes upper in the inheritance chain above a given method.
"
Class {
	#name : 'SycShowLocalSendersCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
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
SycShowLocalSendersCommand >> searchInTheWholeHierarchy [

	^ refactoredClass withAllSuperAndSubclasses 
		flatCollect: [ :class |
			(class thoroughWhichMethodsReferTo: originalMessage selector) , (class class thoroughWhichMethodsReferTo: originalMessage selector) ].
]
