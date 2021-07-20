Extension { #name : #CmdCommandActivator }

{ #category : #'*Calypso-SystemPlugins-Spotter' }
CmdCommandActivator >> doEvaluate [
	
	self executeCommand
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
CmdCommandActivator >> icon [

	^ self menuItemIcon
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
CmdCommandActivator >> label [

	^ self menuItemName
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
CmdCommandActivator >> matchesText: aString [ 

	^ self label = aString
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
CmdCommandActivator >> spotterPreview [

	^ self spotterPreview: SpPresenterBuilder new
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
CmdCommandActivator >> spotterPreview: aBuilder [

	^ aBuilder newText
		beNotEditable;
		text: self command class comment;
		yourself
]
