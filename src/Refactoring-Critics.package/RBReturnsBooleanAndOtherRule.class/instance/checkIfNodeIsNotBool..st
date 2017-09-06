private
checkIfNodeIsNotBool: node
	^ (node isVariable and: [ node name = 'self' ]) or:
		[ node isLiteralNode and: [ (#(true false) includes: node value) not ] ]