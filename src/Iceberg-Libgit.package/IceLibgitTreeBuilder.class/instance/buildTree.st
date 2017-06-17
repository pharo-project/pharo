building
buildTree
	self children keysAndValuesDo: [ :childEntryName :childBuilder |
		childBuilder hasChanges ifTrue: [ 
			self builder 
				addEntry: childBuilder buildTree
				filename: childEntryName
		]
	].

	^ self builder newTree