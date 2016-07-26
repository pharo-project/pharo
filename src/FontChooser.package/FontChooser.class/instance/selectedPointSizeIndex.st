selected
selectedPointSizeIndex

	^self pointSizeList indexOf: (pointSize reduce asString padLeftTo: 3)