initialize-release
fromFillStyles: fillStyles on: aCanvas

	paints := fillStyles collect: [:each | each asAthensPaintOn: aCanvas ]