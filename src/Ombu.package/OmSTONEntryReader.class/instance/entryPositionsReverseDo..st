reading
entryPositionsReverseDo: aBlock

	| found token |
	token := 'OmEntry {'.
	stream setToEnd.

	[	[ 	found := stream backUpTo: token. 
			stream skip: token size negated. ] 
				doWhileTrue: [ found and: [ stream peek = $\ ] ].

		found ifFalse: [ ^self ].

		stream skip: token size negated.
		aBlock value: stream position.
			
	] doWhileTrue: true
