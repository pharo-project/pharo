"
I test Unix symbolic links in ZIP archives: a member whose Unix file type (high bits of the external attributes, with the Unix attribute format) is S_IFLNK, and whose data is the link target.

The fixture was produced by Info-ZIP ""zip -y -X"" from a directory d holding real.txt, a link ""link"" to real.txt, and a link ""evil"" to ../../etc/passwd. By default, links are still extracted as regular files holding their target; a symbolicLinkHandler can be set on the archive to create or skip them.
"
Class {
	#name : 'ZipArchiveSymbolicLinkTest',
	#superclass : 'TestCase',
	#instVars : [
		'fileSystem'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'helpers' }
ZipArchiveSymbolicLinkTest >> infoZipArchive [

	^ ZipArchive new readFrom:
		  'UEsDBAoAAAAAAJp6XF2CxcHmBQAAAAUAAAAKAAAAZC9yZWFsLnR4dGRhdGEKUEsDBAoAAAAAAJp6XF0vMihuCAAAAAgAAAAGAAAAZC9saW5rcmVhbC50eHRQSwMECgAAAAAAmnpcXQNrQN8QAAAAEAAAAAYAAABkL2V2aWwuLi8uLi9ldGMvcGFzc3dkUEsBAh4DCgAAAAAAmnpcXYLFweYFAAAABQAAAAoAAAAAAAAAAQAAAKSBAAAAAGQvcmVhbC50eHRQSwECHgMKAAAAAACaelxdLzIobggAAAAIAAAABgAAAAAAAAAAAAAA7aEtAAAAZC9saW5rUEsBAh4DCgAAAAAAmnpcXQNrQN8QAAAAEAAAAAYAAAAAAAAAAAAAAO2hWQAAAGQvZXZpbFBLBQYAAAAAAwADAKAAAACNAAAAAAA='
			  base64Decoded readStream
]

{ #category : 'helpers' }
ZipArchiveSymbolicLinkTest >> linkMemberNamed: aName target: aTarget [
	"Answer a symbolic link member, as it reads back from a written archive"

	| archive |
	archive := ZipArchive new.
	archive addSymbolicLink: aTarget as: aName.
	^ (self reread: archive) memberNamed: aName
]

{ #category : 'helpers' }
ZipArchiveSymbolicLinkTest >> reread: anArchive [

	| file |
	file := fileSystem / 'archive.zip'.
	file ensureDelete.
	anArchive writeToFile: file.
	^ ZipArchive new readFrom: file
]

{ #category : 'running' }
ZipArchiveSymbolicLinkTest >> setUp [

	super setUp.
	fileSystem := FileSystem memory
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testContainedTargets [

	#( #( 'link' 'real.txt' true ) #( 'd/link' '../real.txt' true ) #( 'd/e/link' '../../x' true ) #( 'd/link' './x/../y' true )
	   #( 'link' '../real.txt' false ) #( 'd/link' '../../etc/passwd' false ) #( 'd/link' 'x/../../../y' false )
	   #( 'link' '/etc/passwd' false ) #( 'link' 'C:\Windows' false ) #( 'link' '\\server\share' false )
	   #( 'link' '' false ) ) do: [ :each |
			| member |
			member := ZipArchive new addSymbolicLink: (each at: 2) as: (each at: 1).
			self assert: member hasContainedSymbolicLinkTarget = (each at: 3) description: (each at: 1) , ' -> ' , (each at: 2) ]
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testDefaultExtractionWritesTargetAsFile [
	"Unchanged behaviour: a link is extracted as a regular file holding its target"

	| directory |
	directory := fileSystem / 'out'.
	directory ensureCreateDirectory.
	self infoZipArchive extractAllTo: directory.
	self assert: (directory / 'd' / 'real.txt') contents equals: 'data' , String lf.
	self assert: (directory / 'd' / 'link') contents equals: 'real.txt'.
	self assert: (directory / 'd' / 'evil') contents equals: '../../etc/passwd'
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testDetectsLinksWrittenByInfoZip [

	| archive |
	archive := self infoZipArchive.
	self deny: (archive memberNamed: 'd/real.txt') isSymbolicLink.
	self assert: (archive memberNamed: 'd/link') isSymbolicLink.
	self assert: (archive memberNamed: 'd/link') symbolicLinkTarget equals: 'real.txt'.
	self assert: (archive memberNamed: 'd/evil') symbolicLinkTarget equals: '../../etc/passwd'
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testHandlerIsNotCalledForLinkNamedOutsideTheDirectory [
	"Zip slip protection also applies to links given to the handler"

	| archive directory handled |
	directory := fileSystem / 'out'.
	directory ensureCreateDirectory.
	archive := ZipArchive new.
	(archive addSymbolicLink: 'target' as: 'link') fileName: '../link'.
	handled := false.
	archive symbolicLinkHandler: [ :member :file | handled := true ].
	self should: [ archive extractMember: archive members first inDirectory: directory overwrite: false ] raise: ZipArchiveError.
	self deny: handled
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testLinkIsPreservedWhenRewritten [

	| member |
	member := (self reread: self infoZipArchive) memberNamed: 'd/link'.
	self assert: member isSymbolicLink.
	self assert: member symbolicLinkTarget equals: 'real.txt'
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testMsdosAttributesAreNeverLinks [
	"The Unix type bits only mean something with the Unix attribute format"

	| member |
	member := ZipArchive new addSymbolicLink: 'target' as: 'link'.
	self assert: member isSymbolicLink.
	member instVarNamed: 'fileAttributeFormat' put: 0.
	self deny: member isSymbolicLink
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testNonAsciiTargetIsStoredInUtf8 [

	| member |
	member := self linkMemberNamed: 'link' target: 'dossier/fiché.txt'.
	self assert: member contents equals: 'dossier/fiché.txt' utf8Encoded.
	self assert: member symbolicLinkTarget equals: 'dossier/fiché.txt'
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testOverwriteExtractionUsesHandler [

	| directory archive handled |
	directory := fileSystem / 'out'.
	directory ensureCreateDirectory.
	archive := self infoZipArchive.
	handled := OrderedCollection new.
	archive symbolicLinkHandler: [ :member :file | handled add: member fileName ].
	archive extractAllTo: directory overwrite: true.
	self assert: handled asArray equals: #( 'd/link' 'd/evil' ).
	self deny: (directory / 'd' / 'link') exists
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testRegularFileHasNoContainedTarget [

	self deny: (self infoZipArchive memberNamed: 'd/real.txt') hasContainedSymbolicLinkTarget
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testSkipSymbolicLinks [

	| directory archive |
	directory := fileSystem / 'out'.
	directory ensureCreateDirectory.
	archive := self infoZipArchive.
	archive skipSymbolicLinks.
	archive extractAllTo: directory.
	self assert: (directory / 'd' / 'real.txt') exists.
	self deny: (directory / 'd' / 'link') exists.
	self deny: (directory / 'd' / 'evil') exists
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testSymbolicLinkHandlerReceivesMemberAndFile [

	| directory archive handled |
	directory := fileSystem / 'out'.
	directory ensureCreateDirectory.
	archive := self infoZipArchive.
	handled := OrderedCollection new.
	archive symbolicLinkHandler: [ :member :file |
			handled add: {
					member fileName.
					file.
					member hasContainedSymbolicLinkTarget } ].
	archive extractAllTo: directory.
	self assert: handled asArray equals: {
			{
				'd/link'.
				(directory / 'd' / 'link').
				true }.
			{
				'd/evil'.
				(directory / 'd' / 'evil').
				false } }.
	self assert: (directory / 'd' / 'real.txt') exists
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testTargetOfRegularFileIsAnError [

	self should: [ (self infoZipArchive memberNamed: 'd/real.txt') symbolicLinkTarget ] raise: ZipArchiveError
]

{ #category : 'tests' }
ZipArchiveSymbolicLinkTest >> testWriteAndReadLink [

	| member |
	member := self linkMemberNamed: 'link' target: 'dir/real.txt'.
	self assert: member isSymbolicLink.
	self assert: member compressionMethod equals: 0.
	self assert: (member unixFileAttributes bitAnd: 8r777) equals: 8r777.
	self assert: member symbolicLinkTarget equals: 'dir/real.txt'
]
