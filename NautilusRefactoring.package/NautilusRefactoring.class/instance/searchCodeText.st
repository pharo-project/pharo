rewrite code
searchCodeText
	^ 'RBParseTreeSearcher new
	matches: ''`@object'' do: [ :node :answer | node ];
	matchesMethod: ''`@method: `@args | `@temps | `@.statements'' do: [ :node :answer | node ];
	yourself'