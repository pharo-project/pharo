generate
preambleForMethod: aNode
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode argumentNames collect: [ :name | RBTemporaryNode named: name].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	^ preamble 