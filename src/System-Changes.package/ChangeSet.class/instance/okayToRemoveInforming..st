testing
okayToRemoveInforming: aBoolean
	"Answer whether it is okay to remove the receiver.  If aBoolean is true, inform the receiver if it is not okay"

	| aName |
	aName := self name.
	self == self class current ifTrue:
		[aBoolean ifTrue: [self inform: 'Cannot remove "', aName, '"
because it is the 
current change set.'].
		^ false].
	^ true
