tests
testSymbols
	| symbols |
	self serializeAndMaterialize: #( #bytes #'' ).
	symbols := Collection allSubclasses collect: [ :each | each name ].
	self serializeAndMaterialize: symbols
