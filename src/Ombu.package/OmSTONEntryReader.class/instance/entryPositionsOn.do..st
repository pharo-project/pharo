reading
entryPositionsOn: aStream do: aBlock

	[
		[ aStream upToAll: 'OmEntry {'.
		aStream peek = $\ ] whileTrue.	"here we handle case when log contains this method itself"

		aStream atEnd
			ifTrue: [ ^ self ]
			ifFalse: [ aBlock value: aStream position - 'OmEntry {' size ]

	] doWhileTrue: true