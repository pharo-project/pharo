accessing
sizeToLineEnd
	| size |
	size := 0.
	self do: [ :span| 
		size := size + span size.
		span isAtLineEnd ifTrue: [ ^ size ]].
	Error signal: 'Should not happen!'.