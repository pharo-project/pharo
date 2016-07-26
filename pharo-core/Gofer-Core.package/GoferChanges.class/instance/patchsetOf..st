queries
patchsetOf: aReference
	"Answer the source snapshot of aReference."
	
	| source target |
	source := self sourceSnapshotOf: aReference.
	target := self targetSnapshotOf: aReference.
	^ target patchRelativeToBase: source