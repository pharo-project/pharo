examples
exampleWithEmbeddedObject

	| text |
	text := TxModel new.
	1000 timesRepeat: [ 
		text endPosition
			insert: 'wioeuhfiw eufi';
			insertObject:  Morph new;
			insert: 'uwe hfiuweh fiue', String cr;
			insert: '-------', String cr.
	].
	^ self openInWindowWithText: text
