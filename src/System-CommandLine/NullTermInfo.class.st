"
I provide a simple mapping from high-level commands to low-level terminal character sequences.

"
Class {
	#name : #NullTermInfo,
	#superclass : #Object,
	#category : #'System-CommandLine-Utilities'
}

{ #category : #'instance creation' }
NullTermInfo >> on: aStream [
	"Null"
	^self
]

{ #category : #TermStyle }
NullTermInfo >> set: aString [
	^ aString
]
