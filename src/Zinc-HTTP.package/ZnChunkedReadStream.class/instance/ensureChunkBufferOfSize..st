private
ensureChunkBufferOfSize: size
	(chunk notNil and: [ size <= chunk size ]) ifTrue: [ ^ self ].  
	chunk := self collectionSpecies new: size.
