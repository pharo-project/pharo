*Slot
layout: aClassLayout
	self
		deprecated: 'Please use #classLayout: instead'
		transformWith: '`@receiver layout: `@statements' -> '`@receiver classLayout: `@statements'.
	self classLayout: aClassLayout