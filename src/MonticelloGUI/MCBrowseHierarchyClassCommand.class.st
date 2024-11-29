Class {
	#name : 'MCBrowseHierarchyClassCommand',
	#superclass : 'MCBrowserAbstractClassCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseHierarchyClassCommand class >> defaultName [

	^ 'Browse hierarchy'
]

{ #category : 'testing' }
MCBrowseHierarchyClassCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self class environment hasClassNamed: self selectedClass ]
]

{ #category : 'executing' }
MCBrowseHierarchyClassCommand >> execute [
	"Browse the selected class"

	(self class environment at: self selectedClass) browseHierarchy
]
