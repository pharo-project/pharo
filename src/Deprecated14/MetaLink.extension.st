Extension { #name : 'MetaLink' }

{ #category : '*Deprecated14' }
MetaLink >> definitionString [

	self deprecated: 'Please don''t use: metalinks have runtime information that cannot be converted safely to strings'.
	^String streamContents: [ :str |
		str nextPutAll: 'MetaLink new'.
		str cr;tab.
		str nextPutAll: 'metaObject: '; nextPutAll: metaObject printString; nextPut: $;.
		str cr;tab.
		str nextPutAll: 'selector: '; nextPutAll: selector printString; nextPut: $;.
		str cr;tab.
		str nextPutAll: 'arguments: '; nextPutAll: arguments printString; nextPut: $;.
		str cr;tab.
		str nextPutAll: 'control: '; nextPutAll: control printString; nextPut: $;.
		self hasMetaLevel ifTrue: [
			str cr;tab.
			str nextPutAll: 'level: '; nextPutAll: level printString; nextPut: $;.
			].
		self hasCondition ifTrue: [
			str cr;tab.
			str nextPutAll: 'condition: '; nextPutAll: condition printString; nextPut: $;.
			].
		str cr;tab.
		str nextPutAll: 'options: '; nextPutAll: options options asArray printString; nextPut: $..
	]
]
