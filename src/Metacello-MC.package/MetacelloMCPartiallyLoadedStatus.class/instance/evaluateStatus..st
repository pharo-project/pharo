evaulation
evaluateStatus: validStatusList
	^self abort
		ifTrue: [ false ]
		ifFalse: [ 
			(self hasNoProject or: [ self vrsnStatus isEmpty ])
				ifTrue: [ true ]
				ifFalse: [ 
					| valid |
					valid := true.
					vrsnStatus
						do: [ :status | 
							(validStatusList includes: status)
								ifFalse: [ valid := false ] ].
					valid ] ]