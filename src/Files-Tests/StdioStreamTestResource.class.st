"
StdioStreamTestResource provides a StdioStream to test for StdioStreamTest.

As redirecting stdio during testing isn't practical a normal file is opened and an instance StdioStream attached to it.  This provides the same result as: 

	pharo test.image < tmpfile.binary 
	pharo test.image > tmpfile.binary

StdioStreams normally cannot be both read and written, and the tests must take that into account.

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	fileReference:		<Object>
	stdioStream:		<Object>


    Implementation Points
"
Class {
	#name : #StdioStreamTestResource,
	#superclass : #TestResource,
	#instVars : [
		'fileReference',
		'contents',
		'fileStream',
		'stdioStream'
	],
	#category : #'Files-Tests-Streams'
}

{ #category : #accessing }
StdioStreamTestResource >> contents [
	^ contents
]

{ #category : #accessing }
StdioStreamTestResource >> fileReference [
	^ fileReference
]

{ #category : #accessing }
StdioStreamTestResource >> fileStream [
	^ fileStream
]

{ #category : #running }
StdioStreamTestResource >> setUp [
	"Create the temporary file and StdioStream"

	| handle |

	fileReference := FileReference newTempFilePrefix: 'StdioStreamTest.' suffix: '.bin'.
	contents := '01234567890123456789'.
	fileStream := fileReference binaryWriteStream wrappedStream.
	fileStream
		nextPutAll: contents;
		position: 0.
	"NOTE: This makes assumptions about the internal structure of BinaryFileStream.
	This shouldn't be done in general."
	handle := fileStream instVarNamed: 'handle'.
	stdioStream := StdioStream handle: handle file: (File named: #stdio) forWrite: true.
]

{ #category : #accessing }
StdioStreamTestResource >> stdioStream [
	^ stdioStream
]

{ #category : #running }
StdioStreamTestResource >> tearDown [
	"Close the receiver's resources"
	
	stdioStream := nil.
	fileStream close.
	fileReference ensureDelete.
]
