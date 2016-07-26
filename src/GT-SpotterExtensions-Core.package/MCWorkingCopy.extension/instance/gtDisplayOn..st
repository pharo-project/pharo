*GT-SpotterExtensions-Core
gtDisplayOn: stream
	ancestry ancestors isEmpty 
		ifTrue: [ 
			super gtDisplayOn: stream. stream nextPutAll: ' (unsaved)' ]
		ifFalse: [ 
			ancestry ancestors first gtDisplayOn: stream.
			self needsSaving ifTrue: [ stream nextPutAll: ' (modified)' ] ]
		