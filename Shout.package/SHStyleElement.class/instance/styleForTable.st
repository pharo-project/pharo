styling
styleForTable
	^ self tokens
		collect: [:token | 
			| style |
			style := OrderedCollection with: token.
			style add: self color.
			self emphasis
				ifNotNil: [style add: self emphasisAsArray].
			style asArray]