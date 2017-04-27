reading
nextEntryPositionIfFound: foundBlock ifNone: noneBlock

	| token |
	token := 'OmEntry {'.
	
	[ 	stream upToAll: token.
		stream peek = $\ ] whileTrue.	"here we handle case when log contains this method itself"

	^stream atEnd
		ifTrue: noneBlock
		ifFalse: [ foundBlock value: stream position - token size ]
