instance creation
fieldName: fieldName value: fieldValue
	^ self
		fieldName: fieldName
		entity: (ZnEntity with: fieldValue)