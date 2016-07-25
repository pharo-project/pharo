miscellaneous
doPass: pass
	"Certain interlace passes are skipped with certain small image
dimensions"

	pass = 1 ifTrue: [ ^ true ].
	((width = 1) and: [height = 1]) ifTrue: [ ^ false ].
	pass = 2 ifTrue: [ ^ width >= 5 ].
	pass = 3 ifTrue: [ ^ height >= 5 ].
	pass = 4 ifTrue: [ ^ (width >=3 ) or: [height >= 5] ].
	pass = 5 ifTrue: [ ^ height >=3 ].
	pass = 6 ifTrue: [ ^ width >=2 ].
	pass = 7 ifTrue: [ ^ height >=2 ].

