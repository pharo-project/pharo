task handling
taskCompleted

	listMorph
	updateList;
	deselectAll.
		
	self removeMorph: progressMorph.
	self addMorphFront: listMorph.
	
	entity ifNotNil: [ ReSystemAnnouncer uniqueInstance
								notifyCritiquesSeen: critiques
								of: entity ].