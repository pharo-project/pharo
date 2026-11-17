Extension { #name : 'FilesSourcesManager' }

{ #category : '*Deprecated14-Kernel' }
FilesSourcesManager class >> default [

	self
		deprecated: 'Use SourcesManager current instead'
		in: #Pharo14
		transformWith: '`@rcv default' -> 'SourcesManager current'.
	^ SourcesManager current
]
