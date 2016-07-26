accessing-computed
rootTallyItems
	| res sons groups |
	res := OrderedCollection new.
	rootMessageTally
		ifNotNil: [ 
			sons := rootMessageTally sonsOver: self threshold.
			groups := sons groupedBy: [ :aTally | aTally process ] having: [ :g | true ].
			groups
				do: [ :g | 
					self reportOtherProcesses
						ifTrue: [ res addAll: g ]
						ifFalse: [ res addAll: (g asSortedCollection reject: [ :gg | gg process isNil ]) ] ] ].
	^ res