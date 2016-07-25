reporting
reportImageParameters: aStream
	self header: 'Image Commandline Parameters' on: aStream.
	self enumerate: [:idx | Smalltalk image argumentAt: idx] on: aStream.