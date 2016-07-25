enumerating
select: selectBlock thenCollect: collectBlock
	"Optimized version of SequenceableCollection>>#select:thenCollect:"

	| newCollection |
	newCollection := self class new.
	self
		do: [ :each | 
			(selectBlock value: each)
				ifTrue: [ newCollection add: (collectBlock value: each) ] ].
	^ newCollection