query
allSentMessages
	"Answer the set of selectors which are sent somewhere in the system."
	| sent |
	sent := IdentitySet new.
	self allBehaviorsDo: [:each | 
				each selectors
					do: [:sel | "Include all sels, but not if sent by self"
						(each compiledMethodAt: sel) literalsDo: [:m | 
								(m isSymbol) ifTrue: ["might be sent"
										m == sel ifFalse: [sent add: m]].
								(m isMemberOf: Array) ifTrue: ["might be performed"
										m do: [:x | (x isSymbol) ifTrue: [x == sel ifFalse: [sent add: x]]]]]]].
		"The following may be sent without being in any literal frame"
		1
			to: Smalltalk specialSelectorSize
			do: [:index | sent
					add: (Smalltalk specialSelectorAt: index)].
	^ sent