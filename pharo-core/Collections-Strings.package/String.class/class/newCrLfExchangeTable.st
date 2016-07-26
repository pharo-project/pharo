initialization
newCrLfExchangeTable
	"a table for exchanging cr with lf and vica versa"
	^ Character allByteCharacters collect: [ :each |
		each = Character cr 
			ifTrue: [ Character lf ]
			ifFalse: [ 
				each = Character lf
					ifTrue: [ Character cr ]
					ifFalse:[ each ]]]