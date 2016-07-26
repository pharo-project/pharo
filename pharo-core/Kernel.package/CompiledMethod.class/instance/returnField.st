accessing
returnField
	"Answer the index of the instance variable returned by a quick return 
	method."
	| prim |
	prim := self primitive.
	prim < 264
		ifTrue: [self error: 'only meaningful for quick-return']
		ifFalse: [^ prim - 264]