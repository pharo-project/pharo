enumerating
reject: rejectBlock thenDo: aBlock
	"Refer to the comment in Collection>>#reject:thenDo:"
	
	|  each |
	
	1 to: self size do: [ :index |
		(rejectBlock value: (each := self at: index))
			ifFalse: [ aBlock value: each ]].