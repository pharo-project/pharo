Class {
	#name : 'MCBrowseMethodVersionsCommand',
	#superclass : 'MCBrowserAbstractMethodCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCBrowseMethodVersionsCommand class >> defaultName [

	^ 'Versions'
]

{ #category : 'testing' }
MCBrowseMethodVersionsCommand >> canBeExecuted [ 

	^ super canBeExecuted and: [ self selectedClassIsLoaded ]
]

{ #category : 'executing' }
MCBrowseMethodVersionsCommand >> execute [
	"Browse versions of the selected method"

	self tools versionBrowser
		browseVersionsForClass: context selectedClassOrMetaClass 
		selector: context selectedMessageName
]
