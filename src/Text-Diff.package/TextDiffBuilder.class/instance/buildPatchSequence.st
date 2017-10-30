creating patches
buildPatchSequence
	"This method is only implemented for backwards compatibility and testing."

	^Array streamContents: [ :stream |
		self 
			patchSequenceDoIfMatch: [ :string |
				stream nextPut: #match -> string ]
			ifInsert: [ :string | 
				stream nextPut: #insert -> string ]
			ifRemove: [ :string | 
				stream nextPut: #remove -> string ] ]