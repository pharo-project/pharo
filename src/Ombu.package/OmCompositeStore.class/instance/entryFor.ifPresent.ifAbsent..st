accessing
entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure
	
	self allStores do: [ :store |
		store 
			entryFor: aReference 
			ifPresent: [ :entry | ^ presentBlockClosure value: entry ]
			ifAbsent: [ ]	 ].
			
	^ absentBlockClosure value