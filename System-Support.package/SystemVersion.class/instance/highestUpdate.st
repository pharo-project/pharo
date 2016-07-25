accessing
highestUpdate
	"Returns the highest number of available updates"

	| sortedUpdates |
	highestUpdate ifNil: [
		sortedUpdates := self updates asSortedCollection.
		highestUpdate := (sortedUpdates isEmpty
			ifTrue: [0]
			ifFalse: [sortedUpdates last])].
	^highestUpdate