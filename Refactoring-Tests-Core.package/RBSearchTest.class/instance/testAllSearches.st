tests
testAllSearches
	classSearches keysAndValuesDo: 
			[:class :searches | 
			class selectors do: 
					[:sel | 
					currentSelector := sel.
					searches 
						do: [:each | each executeTree: (class parseTreeFor: sel) initialAnswer: each answer]]].
	classSearches 
		do: [:searches | searches do: [:each | self assert: each answer isEmpty]]