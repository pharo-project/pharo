tallying
tallyPath: context by: count
	"Tally the context chain"

	| aMethod aTally |
	aMethod := context method.
	aTally := receivers.
	[ aTally isNil ]
		whileFalse: [ 
			aTally method == aMethod
				ifTrue: [ ^ aTally bumpBy: count ].
			aTally := aTally nextLink ].
	aTally := QSystemTally new class: context receiver class method: aMethod.
	aTally nextLink: receivers.
	receivers := aTally.
	^ aTally bumpBy: count