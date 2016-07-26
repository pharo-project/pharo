announcement handling
critiqueBanned: ann

	ann entity = self currentEntity ifTrue: [ 
		self updateMorph ]