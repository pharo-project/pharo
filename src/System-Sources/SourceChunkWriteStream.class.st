"
I am a special ChunkWriteStream used to write on the streams without the flushing part.
If the flushing part can be changed in my super class then my existence must be terminated.
"
Class {
	#name : 'SourceChunkWriteStream',
	#superclass : 'ChunkWriteStream',
	#category : 'System-Sources-Utilities',
	#package : 'System-Sources',
	#tag : 'Utilities'
}

{ #category : 'writing' }
SourceChunkWriteStream >> afterNexPut [

	decoratedStream setToEnd
]
