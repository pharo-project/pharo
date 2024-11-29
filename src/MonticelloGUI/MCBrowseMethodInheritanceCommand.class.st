Class {
	#name : 'MCBrowseMethodInheritanceCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseMethodInheritanceCommand class >> defaultName [

	^ 'Browse implementors'
]

{ #category : 'testing' }
MCBrowseMethodInheritanceCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self selectedClassIsLoaded ]
]

{ #category : 'executing' }
MCBrowseMethodInheritanceCommand >> execute [
	"Browse implementors of the selected method"

	self systemNavigation 
		methodHierarchyBrowserForClass: context selectedClassOrMetaClass 
		selector: context selectedMessageName

]
