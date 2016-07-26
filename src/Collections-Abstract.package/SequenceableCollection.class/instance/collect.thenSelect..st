enumerating
collect: collectBlock thenSelect: selectBlock
	"Optimized version of Collection>>#collect:thenSelect:"
	
	| each |
	
	^  self class new: self size streamContents: [ :stream |
		1 to: self size do: [:index |
			each := collectBlock value: (self at: index).
			(selectBlock value: each)
				ifTrue: [ stream nextPut: each ]]]