actions
accept

	self acceptBasic.
	acceptAction ifNotNil: [acceptAction value: textMorph contents asText].