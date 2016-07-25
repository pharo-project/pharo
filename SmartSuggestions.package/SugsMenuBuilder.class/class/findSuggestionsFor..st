suggestions
findSuggestionsFor: context	
	"Finds the best suggestions for the context passed as a parameter"
	| node suggestions |

	context selectedMethod ifNil: [ ^#() ]. "no support for class pane yet"
	node := self findBestNodeFor: context .
	node ifNil: [ ^OrderedCollection new ].
	suggestions := node ifNotNil: [ node suggestions ].
	suggestions do:[:sugg| sugg context: context].
	^suggestions select: [:sugg | sugg isValidForContext ]
	
	