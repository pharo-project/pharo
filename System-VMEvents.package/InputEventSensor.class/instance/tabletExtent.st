tablet
tabletExtent
	"Answer the full tablet extent in tablet coordinates."

	| params |
	params := self primTabletGetParameters: 1.
	params ifNil: [^ self error: 'no tablet available'].
	^ (params at: 1)@(params at: 2)
