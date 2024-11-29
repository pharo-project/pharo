Class {
	#name : 'MCBrowseFullClassCommand',
	#superclass : 'MCBrowserAbstractClassCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseFullClassCommand class >> defaultName [

	^ 'Browse full'
]

{ #category : 'testing' }
MCBrowseFullClassCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self selectedClassIsLoaded ]
]

{ #category : 'executing' }
MCBrowseFullClassCommand >> execute [
	"Browse the selected class"

	(self class environment at: self selectedClass) browse
]
