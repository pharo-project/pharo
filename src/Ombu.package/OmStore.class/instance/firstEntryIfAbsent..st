accessing
firstEntryIfAbsent: absentBlock
	"Answer the first entry of this store, or execute absentBlock"

	self entriesDo: [ :entry | ^ entry ].

	^ absentBlock value