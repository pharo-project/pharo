tests
testSort
	"self run: #testSort"
	
	| ord |
	ord := OrderedCollection new addAll: #(2 1 3 6 7 10 6); yourself.
	self assert: ord sort asArray = #(1 2 3 6 6 7 10). 
	self assert: ord sort = (OrderedCollection new addAll:#(1 2 3 6 6 7 10); yourself). 
	self assert: (ord sort: [:a :b | a > b]) asArray = #(10 7 6 6 3 2 1).
	ord := OrderedCollection new.
	self assert: ord sort asArray = #(). 
