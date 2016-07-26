spying
spyAllOn: aBlock cutoff: aNumber
	| node result |
	node := self new.
	node reportOtherProcesses: true.	"Irrelevant in this case. All processes will be reported on their own."
	result := node spyAllEvery: self defaultPollPeriod on: aBlock.
	StandardWindow new
		withText:
				(String streamContents: [ :s |  node report: s cutoff: aNumber; close ])
			label: 'Spy Results';
		openInWorld.
	^ result