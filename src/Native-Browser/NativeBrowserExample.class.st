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
	FileSystem workingDirectory openInOSFileBrowser
]

{ #category : #example }
NativeBrowserExample class >> example2 [
	<example>
	OSPlatform current openOSFileBrowserOn: FileLocator home
]

{ #category : #'see class side' }
NativeBrowserExample >> seeClassSide [
]
