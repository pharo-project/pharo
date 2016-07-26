enumerating
reject: rejectBlock thenCollect: collectBlock
	"Optimized version of Collection>>#reject:thenCollect:"
	
	| each |

	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(rejectBlock value: (each := self at: index))
				ifFalse: [ stream nextPut: (collectBlock value: each) ]]]