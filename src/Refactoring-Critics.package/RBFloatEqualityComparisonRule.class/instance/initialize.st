initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`{ :node | node isLiteralNode and: [ node value isFloat ] } = `@expr'
			'`{ :node | node isLiteralNode and: [ node value isFloat ] } ~= `@expr'
			'`@expr = `{ :node | node isLiteralNode and: [ node value isFloat ] }'
			'`@expr ~= `{ :node | node isLiteralNode and: [ node value isFloat ] }' )
		do: [ :node :answer | node ]