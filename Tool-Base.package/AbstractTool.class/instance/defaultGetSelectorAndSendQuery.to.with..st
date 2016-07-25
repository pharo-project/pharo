method
defaultGetSelectorAndSendQuery: array to: queryPerformer with: querySelector
	| selector |
	
	selector := UIManager default request: 'Type selector:' initialAnswer: 'flag:'.
	selector ifNil: [ selector := String new ].
	selector := selector copyWithout: Character space.
	^ selector isEmptyOrNil
		ifFalse: [ 
			(Symbol
				hasInterned: selector
				ifTrue: [ :aSymbol | 
					array at: 1 put: aSymbol.
					queryPerformer perform: querySelector withArguments: array ])
				ifFalse: [ self inform: 'no such selector' ] ]