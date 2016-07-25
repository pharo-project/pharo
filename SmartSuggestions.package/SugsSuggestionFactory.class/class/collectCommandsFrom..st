private
collectCommandsFrom: aPragmaCollector
	| commands |
	commands := SortedCollection
		sortBlock: [ :st :nd | st position <= nd position ].
	aPragmaCollector
		do: [ :prg | 
			commands
				add: (prg methodClass theNonMetaClass perform: prg methodSelector) ].
	^ commands