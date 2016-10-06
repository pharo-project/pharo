enumerating
select: selectBlock thenDo: doBlock
	"Optimized version of SequenceableCollection>>#select:thenDo:"

	self
		do: [ :each | 
				(selectBlock value: each)
					ifTrue: [ doBlock value: each ] ]