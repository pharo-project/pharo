Class {
	#name : 'MCBrowseFullMethodCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseFullMethodCommand class >> defaultName [

	^ 'Browse full'
]

{ #category : 'testing' }
MCBrowseFullMethodCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self selectedClassIsLoaded ]
]

{ #category : 'executing' }
MCBrowseFullMethodCommand >> execute [
	"Browse the selected method"

	self selectedMethod browse
]
