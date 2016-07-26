private
updateEntity: aZnMultiPartFormDataEntity 
	bundles ifNil: [ ^ self ].
	self bundles do: [ :eachEventBundle |
		aZnMultiPartFormDataEntity 
			addPart: (ZnMimePart fieldName: 'category' value: eachEventBundle category);
			addPart: (ZnMimePart fieldName: 'data' entity: eachEventBundle entity) ]