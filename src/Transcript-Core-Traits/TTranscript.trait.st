"
I define the API of Transcript, which is quite similar to stream.

A Transcript acts like a character write stream, accumulating output send to it. Some operations buffer output without necessarily showing it (like #nextPut: nextPutAll: #print: #cr #space and #tab), while others add their output and show any buffered output (like #show: #crShow: and <<). Use #flush or #endEntry to force buffered output to be shown.

Clients are required to implement #critical: #flush: #nextPut: and #nextPutAll: 

Note that #critical: should serialize multithreaded access

Client can optionally implement #close or #clear themselves

Historical note: #ensureCr and #reset were removed since they were not used
"
Trait {
	#name : #TTranscript,
	#category : #'Transcript-Core-Traits'
}

{ #category : #streaming }
TTranscript >> << anObject [
	"Write anObject to the receiver, dispatching using #putOn:
	This is a shortcut for both nextPut: and nextPutAll: since anObject can be both
	the element type of the receiver as well as a collection of those elements.
	No further conversions of anObject are applied.
	Return self to accomodate chaining."

 	anObject putOn: self
]

{ #category : #streaming }
TTranscript >> clear [
	"Clear the receiver, removing all existing output"
]

{ #category : #streaming }
TTranscript >> close [
	"Close the receiver, indicating it is not longer needed"
]

{ #category : #streaming }
TTranscript >> cr [
	"Output a cr on the receiver, buffered and not yet shown"
	
	self nextPut: Character cr
]

{ #category : #streaming }
TTranscript >> crShow: anObject [
	"Output anObject asString on the receiver preceded by a cr and show the output"
	
	self critical: [ self cr; show: anObject ]
]

{ #category : #streaming }
TTranscript >> critical: block [
	"Execute block making sure only one thread accesses the receiver"
	
	self explicitRequirement
]

{ #category : #streaming }
TTranscript >> endEntry [
	"Show the currently buffered output"

	self flush
]

{ #category : #streaming }
TTranscript >> flush [
	"Show the currently buffered output"

	self explicitRequirement
]

{ #category : #streaming }
TTranscript >> nextPut: character [
	"Output character on the receiver, buffered, not yet shown"

	self explicitRequirement
]

{ #category : #streaming }
TTranscript >> nextPutAll: string [
	"Output string on the receiver, buffered, not yet shown"

	self explicitRequirement
]

{ #category : #streaming }
TTranscript >> print: anObject [
	"Output anObject asString on the receiver, buffered, not yet shown"
	
	self nextPutAll: anObject asString
]

{ #category : #streaming }
TTranscript >> show: anObject [
	"Output anObject asString on the receiver and show the output"
	
	self critical: [ self print: anObject; endEntry ]
]

{ #category : #streaming }
TTranscript >> space [
	"Output a space on the receiver, buffered and not yet shown"

	self nextPut: Character space
]

{ #category : #streaming }
TTranscript >> tab [
	"Output a tab on the receiver, buffered and not yet shown"

	self nextPut: Character tab
]
