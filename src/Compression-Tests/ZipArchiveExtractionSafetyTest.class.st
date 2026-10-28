"
I check that extracting a ZipArchive never writes outside of the target directory (zip slip), whatever the member names contain: '..' segments, absolute paths, drive letters or backslashes.
"
Class {
	#name : 'ZipArchiveExtractionSafetyTest',
	#superclass : 'TestCase',
	#instVars : [
		'fs',
		'target',
		'zipFile'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'helpers' }
ZipArchiveExtractionSafetyTest >> assertNothingOutsideTarget [

	self assertEmpty: self filesOutsideTarget
]

{ #category : 'helpers' }
ZipArchiveExtractionSafetyTest >> filesOutsideTarget [

	^ fs root allChildren reject: [ :each |
		  each = fs root or: [ each = zipFile or: [ each = target parent or: [ each = target or: [ target contains: each ] ] ] ] ]
]

{ #category : 'helpers' }
ZipArchiveExtractionSafetyTest >> maliciousNames [

	^ #( '../outside.txt' '....//outside.txt' 'sub/../../outside.txt' 'sub/....//....//outside.txt' '/abs.txt' 'C:/drive.txt'
	     '..\outside.txt' )
]

{ #category : 'helpers' }
ZipArchiveExtractionSafetyTest >> nameReadBackFor: aName [
	"Write an archive with a member named aName and answer the name of that member when the archive is read back"

	| archive |
	self writeArchiveWithMemberNamed: aName.
	archive := ZipArchive new readFrom: zipFile.
	^ [ archive members first fileName ] ensure: [ archive close ]
]

{ #category : 'running' }
ZipArchiveExtractionSafetyTest >> setUp [

	super setUp.
	fs := FileSystem memory.
	target := (fs root / 'base' / 'target')
		          ensureCreateDirectory;
		          yourself.
	zipFile := fs root / 'archive.zip'
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testExtractAllToInformingKeepsFilesInsideTarget [

	self maliciousNames do: [ :each |
			| archive |
			self writeArchiveWithMemberNamed: each.
			archive := ZipArchive new readFrom: zipFile.
			[ archive extractAllTo: target informing: nil ] ensure: [ archive close ].
			self assertNothingOutsideTarget ]
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testExtractAllToOverwriteKeepsFilesInsideTarget [

	self maliciousNames do: [ :each |
			| archive |
			self writeArchiveWithMemberNamed: each.
			archive := ZipArchive new readFrom: zipFile.
			[ archive extractAllTo: target overwrite: true ] ensure: [ archive close ].
			self assertNothingOutsideTarget ]
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testExtractingMemberWithOnlyDotDotRaisesError [
	"'..' becomes an empty name when read, which cannot be extracted as a file"

	| archive |
	self writeArchiveWithMemberNamed: '..'.
	archive := ZipArchive new readFrom: zipFile.
	[ self should: [ archive extractAllTo: target overwrite: true ] raise: ZipArchiveError ] ensure: [ archive close ].
	self assertNothingOutsideTarget
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testInImageDirectoryMemberWithDotDotIsRefused [

	| archive source |
	source := (fs root / 'source')
		          ensureCreateDirectory;
		          yourself.
	archive := ZipArchive new.
	[
		archive addDirectory: source as: '../escaped'.
		self should: [ archive extractAllTo: target informing: nil ] raise: ZipArchiveError.
		self should: [ archive extractAllTo: target overwrite: true ] raise: ZipArchiveError ] ensure: [ archive close ].
	self deny: (target parent / 'escaped') exists
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testInImageMemberWithDotDotIsRefused [
	"Members created in the image are not canonicalized, so extraction itself must refuse to leave the target directory"

	| archive member |
	archive := ZipArchive new.
	[
		member := archive addString: 'pwned' as: '../outside.txt'.
		self should: [ member extractInDirectory: target ] raise: ZipArchiveError.
		self should: [ member extractInDirectory: target withoutInformingOverwrite: true ] raise: ZipArchiveError.
		self should: [ archive extractAllTo: target informing: nil ] raise: ZipArchiveError ] ensure: [ archive close ].
	self assertNothingOutsideTarget
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testReadingKeepsTrailingSlashOfDirectories [

	self assert: (self nameReadBackFor: './dir/../sub/') equals: 'dir/sub/'
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testReadingRemovesDotDotSegments [

	self assert: (self nameReadBackFor: '../outside.txt') equals: 'outside.txt'.
	self assert: (self nameReadBackFor: 'sub/../../outside.txt') equals: 'sub/outside.txt'.
	"Removing '../' only once would turn '....//' into '../'"
	self assert: (self nameReadBackFor: '....//outside.txt') equals: '..../outside.txt'
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testReadingRemovesDotSegments [

	self assert: (self nameReadBackFor: './a/./b.txt') equals: 'a/b.txt'
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testReadingRemovesLeadingSlashesAndDriveLetters [

	self assert: (self nameReadBackFor: '/abs.txt') equals: 'abs.txt'.
	self assert: (self nameReadBackFor: '//abs.txt') equals: 'abs.txt'.
	self assert: (self nameReadBackFor: 'C:/drive.txt') equals: 'drive.txt'
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testReadingTreatsBackslashesAsSeparators [

	self assert: (self nameReadBackFor: '..\outside.txt') equals: 'outside.txt'.
	self assert: (self nameReadBackFor: 'a\b.txt') equals: 'a/b.txt'
]

{ #category : 'tests' }
ZipArchiveExtractionSafetyTest >> testRegularArchiveIsStillExtracted [

	| archive |
	archive := ZipArchive new.
	[
		archive addString: 'top' as: 'top.txt'.
		archive addString: 'nested' as: 'dir/sub/nested.txt'.
		archive writeToFile: zipFile ] ensure: [ archive close ].

	archive := ZipArchive new readFrom: zipFile.
	[ archive extractAllTo: target overwrite: true ] ensure: [ archive close ].

	self assert: (target / 'top.txt') contents equals: 'top'.
	self assert: (target / 'dir' / 'sub' / 'nested.txt') contents equals: 'nested'
]

{ #category : 'helpers' }
ZipArchiveExtractionSafetyTest >> writeArchiveWithMemberNamed: aName [

	| archive |
	zipFile ensureDelete.
	archive := ZipArchive new.
	[
		archive addString: 'pwned' as: aName.
		archive writeToFile: zipFile ] ensure: [ archive close ]
]
