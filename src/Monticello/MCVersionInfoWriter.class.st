"
I serialize an MCVersionInfo into a given stream.
If the version info has been serialized before I output a compacted version consiting of the id.

"
Class {
	#name : #MCVersionInfoWriter,
	#superclass : #MCWriter,
	#instVars : [
		'written'
	],
	#category : 'Monticello-Storing'
}

{ #category : #testing }
MCVersionInfoWriter >> isWritten: aVersionInfo [
	^ self written includes: aVersionInfo
]

{ #category : #serialization }
MCVersionInfoWriter >> writeVersionInfo: aVersionInfo [
	(self isWritten: aVersionInfo)
		ifTrue: [^ stream nextPutAll: '(id '; print: aVersionInfo id asString; nextPut: $) ].
	stream nextPut: $(.
	#(name message id date time author) do: [:sel | 
		stream 
			nextPutAll: sel; space;
			print: (((aVersionInfo perform: sel) ifNil: ['']) asString convertToEncoding: 'latin-1' ); space ].
	stream nextPutAll: 'ancestors ('.
	aVersionInfo ancestors do: [:ea | self writeVersionInfo: ea].
	stream nextPutAll: ') stepChildren ('.
	aVersionInfo stepChildren do: [:ea | self writeVersionInfo: ea].
	stream nextPutAll: '))'.
	
	self wrote: aVersionInfo
]

{ #category : #accessing }
MCVersionInfoWriter >> written [
	^ written ifNil: [written := Set new]
]

{ #category : #serialization }
MCVersionInfoWriter >> wrote: aVersionInfo [
	self written add: aVersionInfo
]
