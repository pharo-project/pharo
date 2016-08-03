accessing
entriesToRedo
	"Answer a sequenced collection of entries to perform a redo operation."

	^ (self itemsForMenuAction  
		flatCollect: [ :item | item entries select: [ :each | each content isCodeChange ] ])
		sorted: [ :a :b | 
			(browser log timeAt: a ifAbsent: [ DateAndTime today ]) 
				<= (browser log timeAt: b ifAbsent: [ DateAndTime today ]) ]