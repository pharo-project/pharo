enumerating
select: selectBlock thenCollect: collectBlock
	"Optimized version of Collection>>#select:thenCollect:"
	
	| each |
	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(selectBlock value: (each := self at: index))
				ifTrue: [ stream nextPut: (collectBlock value: each) ]]]