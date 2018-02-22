"
WindowsFileAttributesPluginPrims provides file attribute functionality specific to the Windows platform, including:

- String encoding and decoding
- Ensuring that symlink information isn't requested

    Implementation Points

On Unix platforms, if attribute information on a broken symbolic link is requested, the symbolic link attribute information is returned instead.  This doesn't make sense on Windows, so ensure the original request simply fails.

"
Class {
	#name : #WindowsFileAttributesPluginPrims,
	#superclass : #FileAttributesPluginPrims,
	#category : #'FileSystem-Disk'
}

{ #category : #'private - encoding' }
WindowsFileAttributesPluginPrims >> decode: aString [
	"On Windows the primitives returned the string with native encoding.
	Just ensure it isn't a ByteArray."

	aString isString ifFalse: [ ^aString asString ].
	^aString
]

{ #category : #'private - encoding' }
WindowsFileAttributesPluginPrims >> encode: aString [
	"On Windows the primitives take native strings"

	^aString ifEmpty: [ 'C:\' ]
]

{ #category : #attributes }
WindowsFileAttributesPluginPrims >> fileAttribute: aString number: attributeNumber [
	"Answer the attribute identified by attributeNumber for the specified file (aString)."

	"Windows doesn'ts support symbolic links"
	attributeNumber = 16 ifTrue: [ ^false ].
	^super fileAttribute: aString number: attributeNumber.

]

{ #category : #'mode bits' }
WindowsFileAttributesPluginPrims >> isSymlink: aPath [
	"Answer a boolean indicating whether the supplied path is a Symlink file"

	^false

]

{ #category : #primitives }
WindowsFileAttributesPluginPrims >> primFileAttribute: aString number: attributeNumber [
	"Answer a single attribute for the supplied file.

stat() information:
	
	1: name
	2: mode
	3: ino
	4: dev
	5: nlink
	6: uid
	7: gid
	8: size
	9: accessDate
	10: modifiedDate
	11: changeDate
	12: creationDate

access() information

	13: is readable
	14: is writeable
	15: is executable

symbolic link information

	16: is symbolic link
	"
	<primitive: 'primitiveFileAttribute' module: 'FileAttributesPlugin' error: error>
	^error isPrimitiveError 
		ifTrue: [ self signalError: error errorCode for: aString ]
		ifFalse: 	[ self primitiveFailed ]
]
