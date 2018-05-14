"
I am a concrete subclass of AbstractBinaryFileStream for stdio streams.

StdioStreams map to one of three types of underlying file: 

- Terminal input/output.
- Piped input/output and named pipes (FIFO files) such as created with the shell pipe character ""|"".
- A file mounted on the file system.
  This includes all the files that can be opened with a FileReference, including sysfs files such as /proc/cpuinfo and character devices such as /dev/urandom.

The operations that can be performed on these vary, e.g. it is possible to position the stream for a regular file, but not for piped input.  Currently it is up to the user of StdioStream to know which type of input they are dealing with.

As pipes can't be positioned and FilePlugin doesn't provide a peek primitive, simulate #peek by reading the next character and holding on to it until it is consumed.

Despite providing both input and output methods, StdioStreams are either read-only or write-only.  Currently it is up to the user to know which type of stream they are dealing with.

Normally instances of StdioStream are not created directly but via Stdio, e.g: 

Stdio stdin.
Stdio stdout.
Stdio stderr.


Instance Variables:

- peekBuffer     <SmallInteger or nil> The next character to be read from the stream or nil.
"
Class {
	#name : #StdioStream,
	#superclass : #AbstractBinaryFileStream,
	#instVars : [
		'peekBuffer'
	],
	#category : #'Files-Streams'
}

{ #category : #accessing }
StdioStream >> next: n [
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read startingAt |
	readBuffer := ByteArray new: n.
	startingAt := 1.
	peekBuffer ifNotNil: [ 
		readBuffer at: 1 put: peekBuffer.
		startingAt := startingAt + 1.
		peekBuffer := nil ].
	read := File read: handle into: readBuffer startingAt: startingAt count: n - startingAt + 1.
	^read = (n - startingAt + 1)
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
]

{ #category : #accessing }
StdioStream >> peek [
	"Answer the next element of the stream, but do not advance the stream pointer. 
	If the receiver is at the end, answer nil."

	self atEnd ifTrue: [ ^ nil ].
	peekBuffer ifNotNil: [ ^ peekBuffer ].
	^ peekBuffer := self next
]
