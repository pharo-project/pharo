encoding
encodeSelector: aSelector

	^self
		name: aSelector
		key: aSelector
		class: SelectorNode
		type: SendType
		set: selectorSet