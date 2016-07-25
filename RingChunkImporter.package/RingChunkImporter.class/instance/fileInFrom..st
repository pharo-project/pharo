reading
fileInFrom: aStream

	| changes |
	changes := (CodeImporter readStream: aStream) parseChunks.
	changes do: [ :change | change accept: self ].