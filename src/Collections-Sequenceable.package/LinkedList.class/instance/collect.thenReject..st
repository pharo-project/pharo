enumerating
collect: collectBlock thenReject: rejectBlock
	"Optimized version of SequenceableCollection>>#collect:#thenReject:"

	| newCollection newElement |
	newCollection := self class new.
	self
		do: [ :each | 
			newElement := collectBlock value: each.
			(rejectBlock value: newElement)
				ifFalse: [ newCollection add: newElement ] ].
	^ newCollection