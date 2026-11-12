"
Classify the selected methods
"
Class {
	#name : 'SycClassifyMethodsCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycClassifyMethodsCommand >> description [

	^ 'Classify the selected methods'
]

{ #category : 'execution' }
SycClassifyMethodsCommand >> execute [

	| aProtocol |
	aProtocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: context lastSelectedClass ].

	aProtocol ifNil: [ ^ self ].
	self methods do: [ :aMethod | aMethod protocol: aProtocol ].
	context showProtocol: aProtocol
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> iconName [

	^ #browse
]

{ #category : 'accessing' }
SycClassifyMethodsCommand >> name [

	^ 'Classify methods'
]
