private
reallyBind: name

	| node |
	node := self newTemp: name.
	scopeTable at: name put: node.
	^node