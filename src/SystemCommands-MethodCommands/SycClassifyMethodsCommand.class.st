"
Classify the selected methods
"
Class {
	#name : 'SycClassifyMethodsCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#instVars : [
		'protocol'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycClassifyMethodsCommand >> applyCommandResult [

	self methods do: [ :aMethod | aMethod protocol: self protocol ].
	context showProtocol: self protocol
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> icon [

	^ self iconNamed: #browse
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> name [

	^ 'Classify methods'
]

{ #category : 'preparation' }
SycClassifyMethodsCommand >> prepareFullExecution [

	super prepareFullExecution.
	protocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: context lastSelectedClass ]
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> protocol [

	^ protocol
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> protocol: anObject [

	protocol := anObject
]
