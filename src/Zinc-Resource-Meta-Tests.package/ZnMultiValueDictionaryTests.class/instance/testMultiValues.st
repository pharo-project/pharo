testing
testMultiValues
	| dictionary values keys |
	dictionary := ZnMultiValueDictionary new.
	dictionary at: 'foo' add: 1; at: 'foo' add: 2.
	self assert: (dictionary at: 'foo') = #(1 2).
	self assert: dictionary keys asArray = #('foo').
	values := OrderedCollection new.
	keys := OrderedCollection new.
	dictionary keysAndValuesDo: [ :key :value | keys add: key. values add: value ].
	self assert: values = (OrderedCollection with: 1 with: 2).
	self assert: keys = (OrderedCollection with: 'foo' with: 'foo')
	