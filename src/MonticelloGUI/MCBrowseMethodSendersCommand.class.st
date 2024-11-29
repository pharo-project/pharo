Class {
	#name : 'MCBrowseMethodSendersCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseMethodSendersCommand class >> defaultName [

	^ 'Browse senders'
]

{ #category : 'executing' }
MCBrowseMethodSendersCommand >> execute [
	"Browse senders of the selected method"

	self selectedMethod browseSenders
]
