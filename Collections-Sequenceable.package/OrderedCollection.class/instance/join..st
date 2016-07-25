splitjoin
join: aCollection 
	| result |
	result := self class new.
	aCollection
		do: [:each | each appendTo: result]
		separatedBy: [self appendTo: result].
	^ result