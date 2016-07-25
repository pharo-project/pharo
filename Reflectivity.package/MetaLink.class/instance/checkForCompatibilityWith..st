installing
checkForCompatibilityWith: aNode
	| supported |
	
	supported := Set new.
	RFReification subclasses do: [:plugin |
		(plugin entities anySatisfy: [:class | aNode isKindOf: class]) ifTrue: [
		supported add: plugin key]].
	self allReifications do: [ :each | (supported includes: each) ifFalse: [^false  ]  ].
	^true