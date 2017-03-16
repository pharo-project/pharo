reading
entryPositionsDo: aBlock

	[[ stream upToAll: 'OmEntry {'.
		stream peek = $\ ] whileTrue.	"here we handle case when log contains this method itself"

		stream atEnd
			ifTrue: [ ^ self ]
			ifFalse: [ aBlock value: stream position - 'OmEntry {' size ]

	] doWhileTrue: true