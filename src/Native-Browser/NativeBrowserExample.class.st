"
Description
--------------------

Example of Native-Browser usage
"
Class {
	#name : #NativeBrowserExample,
	#superclass : #Object,
	#category : #'Native-Browser-Example'
}

{ #category : #example }
NativeBrowserExample class >> example [
	<example>
	FileSystem workingDirectory openInNativeBrowser
]

{ #category : #example }
NativeBrowserExample class >> example2 [
	<example>
	OSPlatform current openNativeBrowserOn: FileLocator home
]

{ #category : #'see class side' }
NativeBrowserExample >> seeClassSide [
]
