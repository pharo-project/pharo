"
I am a utility for replacing the normal Stdio during tests and maybe for activating Clap commands from a workspace.
"
Class {
	#name : 'ClapPluggableStdio',
	#superclass : 'Object',
	#instVars : [
		'in',
		'out',
		'err'
	],
	#category : 'Clap-Core-Activation',
	#package : 'Clap-Core',
	#tag : 'Activation'
}

{ #category : 'instance creation' }
ClapPluggableStdio class >> onByteArrays [
	^ self onByteArraysWithInputBytes: #[]
]

{ #category : 'instance creation' }
ClapPluggableStdio class >> onByteArraysWithInput: aString [
	^ self onByteArraysWithInputBytes: aString utf8Encoded
]

{ #category : 'instance creation' }
ClapPluggableStdio class >> onByteArraysWithInputBytes: aByteArray [
	^ self new
		initializeInput: aByteArray readStream
		output: #[] writeStream
		error: #[] writeStream
]

{ #category : 'initialization' }
ClapPluggableStdio >> initializeInput: inputStream output: outputStream error: errorStream [
	in := inputStream.
	out := outputStream.
	err := errorStream
]

{ #category : 'accessing' }
ClapPluggableStdio >> stderr [
	^ err
]

{ #category : 'accessing' }
ClapPluggableStdio >> stdin [
	^ in
]

{ #category : 'accessing' }
ClapPluggableStdio >> stdout [
	^ out
]
