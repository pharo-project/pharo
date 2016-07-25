private html
renderAttributes: attributes
	attributes pairsDo: [ :name :value | 
		stream space; nextPutAll: name.
		value ifNotNil: [
			stream nextPutAll: '="'.
			self escapeAttributeValue: value asString.
			stream nextPut: $" ] ]