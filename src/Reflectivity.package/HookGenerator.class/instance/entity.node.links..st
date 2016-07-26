initialize
entity: anEntity node: aNode links: aCollection
	plugins := Dictionary new.
	node := aNode.
	entity := anEntity.
	links := aCollection.
	
	"register all the plugins that can reify information for this node"
	RFReification subclasses do: [:plugin |
		(plugin entities anySatisfy: [:class | entity isKindOf: class])	 ifTrue: [
			plugins at: plugin key put: plugin]
		].
