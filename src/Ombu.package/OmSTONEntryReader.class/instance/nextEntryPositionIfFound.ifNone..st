reading
nextEntryPositionIfFound: foundBlock ifNone: noneBlock

	| token |
	token := 'OmEntry {'.

	[ [ stream upToAll: token ]
		on: UTF8InvalidText
		do: [ :exception | 
			"Workaround: positioned block in middle of multibyte utf8 char 
			   => skip a byte then try again"
			stream basicNext.
			exception retry ].
	stream peek = $\ ] whileTrue.	"here we handle case when log contains this method itself"

	^ stream atEnd
		ifTrue: noneBlock
		ifFalse: [ foundBlock value: stream position - token size ]