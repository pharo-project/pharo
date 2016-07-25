enumerating
collect: collectBlock thenSelect: selectBlock
	"Optimized version of SequenceableCollection>>#collect:#thenSelect:"

	| newCollection newElement |
	newCollection := self class new.
	self
		do: [ :each | 
			newElement := collectBlock value: each.
			(selectBlock value: newElement)
				ifTrue: [ newCollection add: newElement ] ].
	^ newCollection