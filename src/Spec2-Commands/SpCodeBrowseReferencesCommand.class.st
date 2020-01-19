Class {
	#name : #SpCodeBrowseReferencesCommand,
	#superclass : #SpCodeBrowsingCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeBrowseReferencesCommand class >> defaultIconName [
	^#smallSystemBrowser 
]

{ #category : #default }
SpCodeBrowseReferencesCommand class >> defaultName [

	^ 'Browse references'
]

{ #category : #executing }
SpCodeBrowseReferencesCommand >> execute [
	self browserClientProvider browseAllReferencesTo: self selector
]
