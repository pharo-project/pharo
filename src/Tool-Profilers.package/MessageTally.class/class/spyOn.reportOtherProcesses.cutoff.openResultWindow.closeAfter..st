spying
spyOn: aBlock reportOtherProcesses: aBoolean cutoff: aNumber openResultWindow: openResultWindow closeAfter: closeAfter
	| node |
	node := self new.
	node reportOtherProcesses: aBoolean.
	node spyEvery: self defaultPollPeriod on: aBlock.
	openResultWindow
		ifTrue: [ 
			StandardWindow new
				withText: (String streamContents: [ :s | node report: s cutoff: aNumber ]) label: 'Spy Results';
				openInWorld ].
	closeAfter
		ifTrue: [ node close ].
	^ node