testing
hasWidthRestrictions: aBrick
	^ aBrick layout hasMinWidth or: [ aBrick layout hasMaxWidth ]