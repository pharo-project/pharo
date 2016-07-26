public-enumerating
entriesAt: aResolvable
	^ Array
		streamContents: [ :out | 
			self entriesAt: aResolvable do: [ :entry | out nextPut: entry ]
		]