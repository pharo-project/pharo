Class {
	#name : #SpCodeBrowseMethodsContainingStringCommand,
	#superclass : #SpCodeBrowsingCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeBrowseMethodsContainingStringCommand class >> defaultIconName [
	^#smallSystemBrowser 
]

{ #category : #default }
SpCodeBrowseMethodsContainingStringCommand class >> defaultName [

	^ 'Browse methods containing string'
]

{ #category : #executing }
SpCodeBrowseMethodsContainingStringCommand >> execute [
	self browserClientProvider browseMethodsWithSourceString: self selection matchCase: false
]
