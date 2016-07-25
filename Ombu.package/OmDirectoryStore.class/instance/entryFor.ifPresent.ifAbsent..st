accessing
entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure
	
	^ compositeStore 
		entryFor: aReference 
		ifPresent: presentBlockClosure
		ifAbsent: [ 
			self 
				addStoreNamed: aReference globalName
				ifPossible: [ :addedStore |
					addedStore
						entryFor: aReference 
						ifPresent: presentBlockClosure
						ifAbsent: absentBlockClosure ]
				ifNotPossible: absentBlockClosure ]