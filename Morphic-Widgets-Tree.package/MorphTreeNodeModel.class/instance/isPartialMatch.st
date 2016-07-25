testing
isPartialMatch
	| result |

	result := false.
	
	self contents do: [:each | 
		(each selected or: [ each isPartialMatch ]) ifTrue: [ result := true ] ].

	^ result