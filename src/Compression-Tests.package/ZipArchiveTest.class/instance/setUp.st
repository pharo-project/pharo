running
setUp

	| now |
	now := DateAndTime now asString replaceAll: $: with: $_.
	fileToZip := ('_test-zip-file', now) asFileReference.
	fileToZip writeStreamDo: [ :stream | stream nextPutAll: 'file contents' ].
	subdir := ('_test-zip-dir', now) asFileReference.
	subdir ensureCreateDirectory.
	zipFile := ('pharo_test', now) asFileReference, 'zip'.
	zip := ZipArchive new.
