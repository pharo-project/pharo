accessing
pointSizeList

	pointSizeList ifNotNil: [ ^ pointSizeList ].
	^ pointSizeList := (1 to: 256) collect: [ :each | each asString padLeftTo: 3 ].