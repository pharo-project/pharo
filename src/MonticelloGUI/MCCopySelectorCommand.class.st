Class {
	#name : 'MCCopySelectorCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCCopySelectorCommand class >> defaultName [

	^ 'Copy selector'
]

{ #category : 'executing' }
MCCopySelectorCommand >> execute [
	"Browse the selected method"

	Clipboard 
		clipboardText: self selectedMethod selector 
		informing: ('Selector {1} copied to clipboard' format: { self selectedMethod selector })
]
