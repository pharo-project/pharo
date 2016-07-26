creating patches
buildDisplayPatch

	^Text streamContents: [ :stream |
		self 
			patchSequenceDoIfMatch: [ :string |
				self print: string withAttributes: NormalTextAttributes on: stream ]
			ifInsert: [ :string |
				self print: string withAttributes: InsertTextAttributes on: stream ]
			ifRemove: [ :string |
				self print: string withAttributes: RemoveTextAttributes on: stream ] ]