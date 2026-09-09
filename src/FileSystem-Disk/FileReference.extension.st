Extension { #name : 'FileReference' }

{ #category : '*FileSystem-Disk' }
FileReference class >> newTempFilePrefix: prefix suffix: suffix [
	| tmpDir random fileName |

	tmpDir := FileLocator temp asFileReference.
	[
		random := UUID new asInteger asString.
		fileName := prefix , random , suffix.
		(tmpDir / fileName) exists ] whileTrue.
	^ tmpDir / fileName
]
