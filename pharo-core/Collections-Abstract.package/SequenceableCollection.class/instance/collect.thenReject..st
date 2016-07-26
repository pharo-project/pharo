enumerating
collect: collectBlock thenReject: rejectBlock
	"Optimized version of Collection>>#thenReject:"
	
	| each |
	
	^  self class new: self size streamContents: [ :stream |
		1 to: self size do: [:index |
			each := collectBlock value: (self at: index).
			(rejectBlock value: each)
				ifFalse: [ stream nextPut: each ]]]