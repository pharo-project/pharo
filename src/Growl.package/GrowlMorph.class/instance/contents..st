building
contents: contentsString

	self streamDo: [ :w |
		w withAttributes: self contentsAttributes do: [w nextPutAll: contentsString asString]].