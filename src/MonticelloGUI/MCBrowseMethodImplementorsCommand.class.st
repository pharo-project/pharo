Class {
	#name : 'MCBrowseMethodImplementorsCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseMethodImplementorsCommand class >> defaultName [

	^ 'Browse inheritance'
]

{ #category : 'testing' }
MCBrowseMethodImplementorsCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self selectedClassIsLoaded ]
]

{ #category : 'executing' }
MCBrowseMethodImplementorsCommand >> execute [
	"Browse implementors of the selected method"

	self systemNavigation browseAllImplementorsOf: (context selectedMessageName ifNil: [ ^nil ])
]
