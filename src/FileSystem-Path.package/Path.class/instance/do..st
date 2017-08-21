enumerating
do: aBlock 
	1 
		to: self size
		do: 
			[ :index || segment |
			segment := self at: index.
			segment isEmpty ifFalse: [ aBlock value: segment ] ]