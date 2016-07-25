enumerating
select: selectBock thenDo: aBlock
	"Refer to the comment in Collection>>#select:thenDo:"
	
	| each |
	1 to: self size do: [ :index |
		(selectBock value: (each := self at: index))
			ifTrue: [ aBlock value: each ]].