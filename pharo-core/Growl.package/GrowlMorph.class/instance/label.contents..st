building
label: labelString contents: contentsString

	self streamDo: [ :w |
		w withAttributes: self labelAttributes do: [w nextPutAll: labelString asString; cr].
		w withAttributes: self contentsAttributes do: [w nextPutAll: contentsString asString].
		].