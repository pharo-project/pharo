tests parsing
testLiteralArray
	| tree collection |
	tree := self parserClass parseExpression: '#( a #b #''c'' . + - 1 -2 3.4 #true true #false false #nil nil "comment" ''string'' #[ 1 2 3 ] #(1 2 3))'.
	collection := OrderedCollection new.
	collection add: #a; add: #b; add: #c; add: #'.'; add: #+; add: #-; add: 1; add: -2; add: 3.4; add: #true; add: true; add: #false; add: false; add: #nil; add: nil; add: 'string'; add: #[1 2 3]; add: #(1 2 3).	
	tree value with: collection do: [ :token :value | 
		self assert: token value = value ]