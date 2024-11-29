Class {
	#name : 'MCBrowseHierarchyMethodCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseHierarchyMethodCommand class >> defaultName [

	^ 'Browse hierarchy...'
]

{ #category : 'testing' }
MCBrowseHierarchyMethodCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self selectedClassIsLoaded ]
]

{ #category : 'executing' }
MCBrowseHierarchyMethodCommand >> execute [
	"Browse the selected method"

	self systemNavigation 
		browseHierarchy: context selectedClassOrMetaClass
		selector: context selectedMessageName	
]
