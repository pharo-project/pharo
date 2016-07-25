instance creation
openOnStream: aStream

	| widget |
	widget := self new.
	widget changes items: (CodeImporter chunksFromStream: aStream).
	widget openWithSpec	