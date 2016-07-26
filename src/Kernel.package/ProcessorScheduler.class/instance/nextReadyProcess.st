CPU usage tally
nextReadyProcess
	quiescentProcessLists reverseDo: [ :list |
		list isEmpty ifFalse: [ | proc |
			proc := list first.
			proc suspendedContext ifNotNil: [ ^proc ]]].
	^nil