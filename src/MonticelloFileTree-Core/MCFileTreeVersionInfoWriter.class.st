"
Write for the version info
"
Class {
	#name : #MCFileTreeVersionInfoWriter,
	#superclass : #MCVersionInfoWriter,
	#category : #'MonticelloFileTree-Core'
}

{ #category : #serialization }
MCFileTreeVersionInfoWriter >> writeVersionInfo: aVersionInfo [
	(self isWritten: aVersionInfo)
		ifTrue: [^ stream nextPutAll: '(id '; print: aVersionInfo id asString; nextPut: $) ].
	stream nextPut: $(.
	#(name message id date time author) do: [:sel | 
		stream 
			nextPutAll: sel; space;
			print: (((aVersionInfo perform: sel) ifNil: ['']) asString  ); space ].
	stream nextPutAll: 'ancestors ('.
	aVersionInfo ancestors do: [:ea | self writeVersionInfo: ea].
	stream nextPutAll: ') stepChildren ('.
	aVersionInfo stepChildren do: [:ea | self writeVersionInfo: ea].
	stream nextPutAll: '))'.
	
	self wrote: aVersionInfo
]
