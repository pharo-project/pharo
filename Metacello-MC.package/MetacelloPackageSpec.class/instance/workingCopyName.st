gofer
workingCopyName

	| wc |
	(wc := self workingCopy) == nil ifTrue: [ ^nil ].
	wc ancestry ancestors isEmpty not
		ifTrue: [ ^wc ancestry ancestors first name ].
	^nil