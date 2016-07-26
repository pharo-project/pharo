enumerating
reject: rejectBlock 
	"Optimized version of Collection>>#reject:"
	
	| each |
	
	^ self class new: self size streamContents: [ :stream|
		1 to: self size do: [ :index |
			(rejectBlock value: (each := self at: index))
				ifFalse: [ stream nextPut: each ]]]