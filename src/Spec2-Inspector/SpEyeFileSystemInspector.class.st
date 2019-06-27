"
I am an inspector dedicated to the visualization of file systems
"
Class {
	#name : #SpEyeFileSystemInspector,
	#superclass : #SpEyeTreeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
SpEyeFileSystemInspector class >> label [
	^ 'FileReference Tree Inspector'
]

{ #category : #api }
SpEyeFileSystemInspector >> childrenForObject: aFileReference [
	aFileReference isDirectory ifFalse: [^#()].
	^ aFileReference children collect: [ :each |
		SpDynamicEyeElement
			host: aFileReference
			value: each
		 ]
]

{ #category : #api }
SpEyeFileSystemInspector >> roots [
	^ self childrenForObject: self object
]
