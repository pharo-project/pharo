"
Classify the selected methods
"
Class {
	#name : 'SycClassifyMethodsCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycClassifyMethodsCommand >> execute [

	| aProtocol |
	aProtocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: context lastSelectedClass ].

	aProtocol ifNil: [ ^ self ].
	self methods do: [ :aMethod | aMethod protocol: aProtocol ].
	context showProtocol: aProtocol
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> icon [

	^ self iconNamed: #browse
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> name [

	^ 'Classify methods'
]
