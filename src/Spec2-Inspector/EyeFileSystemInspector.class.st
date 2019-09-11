"
I am an inspector dedicated to the visualization of file systems
"
Class {
	#name : #EyeFileSystemInspector,
	#superclass : #EyeTreeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeFileSystemInspector class >> label [
	^ 'FileReference Tree Inspector'
]

{ #category : #api }
EyeFileSystemInspector >> childrenForObject: aFileReference [
	aFileReference isDirectory ifFalse: [^#()].
	^ aFileReference children collect: [ :each |
		DynamicEyeElement
			host: aFileReference
			value: each
		 ]
]

{ #category : #api }
EyeFileSystemInspector >> roots [
	^ self childrenForObject: self object
]
