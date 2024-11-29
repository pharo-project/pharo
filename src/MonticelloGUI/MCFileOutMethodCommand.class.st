Class {
	#name : 'MCFileOutMethodCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCFileOutMethodCommand class >> defaultName [

	^ 'File out'
]

{ #category : 'executing' }
MCFileOutMethodCommand >> execute [
	"File out the selected method"

	context fileOutMessage
]
