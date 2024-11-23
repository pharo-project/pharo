"
Classify the selected methods
"
Class {
	#name : 'SycClassifyMthCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#instVars : [
		'protocol'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycClassifyMthCommand >> applyCommandResult [

	context showProtocol: protocol
]

{ #category : 'executing' }
SycClassifyMthCommand >> execute [

	self methods do: [ :aMethod | aMethod protocol: self protocol ].

]

{ #category : 'accessing' }
SycClassifyMthCommand >> icon [

	^ self iconNamed: #browse
]

{ #category : 'accessing' }
SycClassifyMthCommand >> name [

	^ 'Classify methods'
]

{ #category : 'preparation' }
SycClassifyMthCommand >> prepareFullExecution [

	super prepareFullExecution.
	protocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: context lastSelectedClass ]
]

{ #category : 'accessing' }
SycClassifyMthCommand >> protocol [

	^ protocol
]

{ #category : 'accessing' }
SycClassifyMthCommand >> protocol: anObject [

	protocol := anObject
]
