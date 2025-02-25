"
I am a path to express UNC paths e.g.,	
	#( '\\MachineName.x.y.z\directory\file' '\\MachineName.x.y.z\directory' '\\MachineName.x.y.z' ) .

This is useful to represent files in SMB shares as used by Windows or Samba.

A NetworkPath acts as an absolute path but with a different translation to string.
"
Class {
	#name : 'UNCNetworkPath',
	#superclass : 'Path',
	#type : 'variable',
	#category : 'FileSystem-Path-Base',
	#package : 'FileSystem-Path',
	#tag : 'Base'
}

{ #category : 'testing' }
UNCNetworkPath >> isAbsolute [

	^ true
]

{ #category : 'testing' }
UNCNetworkPath >> isNetworkPath [

	^ true
]

{ #category : 'testing' }
UNCNetworkPath >> isRoot [

	"A network path is a root if it only has the machine name, it is of the form \\machine"
	^ self size = 1
]

{ #category : 'printing' }
UNCNetworkPath >> printOn: aStream [
	aStream nextPutAll: 'Path ''//'.

	self segments do: [ :e | aStream nextPutAll: e ]
		separatedBy: [ aStream nextPutAll: '/' ].

	aStream nextPut: $'
]

{ #category : 'printing' }
UNCNetworkPath >> printOn: aStream delimiter: aCharacter [

	aStream nextPut: aCharacter.
	aStream nextPut: aCharacter.
	super printOn: aStream delimiter: aCharacter
]

{ #category : 'printing' }
UNCNetworkPath >> printPathOn: aStream delimiter: aCharacter [
	"Print the path elements of the receiver, without the 'Path *' part"

	aStream nextPut: aCharacter.
	aStream nextPut: aCharacter.
	super printPathOn: aStream delimiter: aCharacter
]

{ #category : 'enumerating' }
UNCNetworkPath >> withParents [

	^ super withParents
]
