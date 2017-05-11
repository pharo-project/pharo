accessing
lostChanges

	| freshLog lastSessionHeadReference entries |
	self hasLostChanges ifFalse: [ ^ #() ]. "Start by fastest available check: file size"
	lastSessionHeadReference := lastSessionFileStore headReference.
	lastSessionFileStore refresh.
	freshLog := EpLog newWithStore: lastSessionFileStore.
	freshLog headReference = lastSessionHeadReference ifTrue: [ ^ #() ]. "Just no lost events"

	"Then, there are lost events"
	entries := freshLog priorEntriesFrom: freshLog headReference upTo: lastSessionHeadReference.

	^ entries ifNotEmpty: [ entries allButLast "reject lastSessionHeadReference's change"]