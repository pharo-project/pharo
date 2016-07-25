wiw support
morphicLayerNumber

	"helpful for insuring some morphs always appear in front of or behind others.
	smaller numbers are in front"

	^(owner isNil or: [owner isWorldMorph]) ifTrue: [
		self valueOfProperty: #morphicLayerNumber ifAbsent: [100]
	] ifFalse: [
		owner morphicLayerNumber
	].

	"leave lots of room for special things"