enumerating
groupBy: keyBlock having: selectBlock 
	"This message is deprecated. Please use groupedBy:having:."

	self
		deprecated: 'Please use #groupedBy:having: instead'
		transformWith: '`@receiver groupBy: `@statements1 having: `@statements2' 
						-> '`@receiver groupedBy: `@statements1 having: `@statements2'.

	^ self groupedBy: keyBlock having: selectBlock 