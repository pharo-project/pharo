Extension { #name : #CmdCommandActivator }

{ #category : #'*Calypso-SystemPlugins-Spotter' }
CmdCommandActivator >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	
	^aComposite text
		title: [self command class name]; 
		display: [self command class comment];
		entity: [ self ]
]
