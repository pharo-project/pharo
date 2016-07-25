services
reorderChangeSets
	"Change the order of the change sets to something more convenient:
		First come all numbered updates.
		Next come all remaining changesets"

	"self reorderChangeSets"
	
	| newMid newTail |
	newMid := OrderedCollection new.
	newTail := OrderedCollection new.
	self allChangeSets do:
		[:aChangeSet |
			aChangeSet isNumbered
						ifTrue: [newMid add: aChangeSet]
						ifFalse: [newTail add: aChangeSet]].
	self  allChangeSets: newMid, newTail.
	