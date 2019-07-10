Extension { #name : #Morph }

{ #category : #'*Commander-Activators-Mouse' }
Morph >> enableAllMouseCommandsFrom: someObjectWithCommandContext [
	
	self
		enableMouseCommands: CmdClickActivation withContextFrom: someObjectWithCommandContext;
		enableMouseCommands: CmdDoubleClickActivation withContextFrom: someObjectWithCommandContext
]

{ #category : #'*Commander-Activators-Mouse' }
Morph >> enableMouseCommands: aMouseActivationClass withContextFrom: someObjectWithCommandContext [

	aMouseActivationClass enableInMorph: self withCommandsFrom: someObjectWithCommandContext
]
