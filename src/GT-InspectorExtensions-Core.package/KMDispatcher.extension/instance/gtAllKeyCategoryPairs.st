*GT-InspectorExtensions-Core
gtAllKeyCategoryPairs

	^ ((self targets collect: #category)
			add: self directKeymaps;
			yourself)
			flatCollect: [ :category | 
				category allEntries keymaps collect: [ :key | key -> category ] ]