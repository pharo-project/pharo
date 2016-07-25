creating patches
buildPatchSequence
	"This method is only implemented for backwards compatibility and testing."

	^Array streamContents: [ :stream |
		self 
			patchSequenceDoIfMatch: [ :string |
				stream nextPut: #match -> (string copyWithout: Character cr) ]
			ifInsert: [ :string | 
				stream nextPut: #insert -> (string copyWithout: Character cr) ]
			ifRemove: [ :string | 
				stream nextPut: #remove -> (string copyWithout: Character cr) ] ]