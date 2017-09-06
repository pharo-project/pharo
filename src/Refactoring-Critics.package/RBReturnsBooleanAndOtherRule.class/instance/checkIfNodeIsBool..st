private
checkIfNodeIsBool: node
	^ (node isLiteralNode and: [ #(true false) includes: node value ])
		or: [ node isMessage and: [ #(and: or:) includes: node selector ] ]