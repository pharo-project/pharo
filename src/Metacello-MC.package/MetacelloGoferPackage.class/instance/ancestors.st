querying
ancestors

	| wc |
	(wc := self workingCopy) ~~ nil
		ifTrue: [ 
			wc ancestry ancestors isEmpty not
				ifTrue: [ ^wc ancestry ancestors ]].
	^nil