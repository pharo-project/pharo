"
This command find the senders of a given method in the classes upper in the inheritance chain above a given method.
"
Class {
	#name : 'SycHierarchicalSendersCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycHierarchicalSendersCommand >> applyResultInContext: a [
]

{ #category : 'accessing' }
SycHierarchicalSendersCommand >> defaultMenuIconName [

	^ #edit
]

{ #category : 'accessing' }
SycHierarchicalSendersCommand >> defaultMenuItemName [

	^ 'Hierarchical senders'
]

{ #category : 'execution' }
SycHierarchicalSendersCommand >> execute [

	Smalltalk tools messageList browse: self searchInTheWholeHierarchy.
]

{ #category : 'testing' }
SycHierarchicalSendersCommand >> isComplexRefactoring [

	^ false
]

{ #category : 'execution' }
SycHierarchicalSendersCommand >> searchInTheWholeHierarchy [

	^ refactoredClass withAllSuperAndSubclasses 
		flatCollect: [ :class |
			(class thoroughWhichMethodsReferTo: originalMessage selector) , (class class thoroughWhichMethodsReferTo: originalMessage selector) ].
]
