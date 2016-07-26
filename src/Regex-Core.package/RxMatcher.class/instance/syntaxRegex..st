double dispatch
syntaxRegex: regexNode
	"Double dispatch from the syntax tree. 
	Regex node is a chain of branches to be tried. Should compile this 
	into a bundle of parallel branches, between two marker nodes." 
	
	| startIndex endIndex endNode alternatives |
	startIndex := self allocateMarker.
	endIndex := self allocateMarker.
	endNode := RxmMarker new index: endIndex.
	alternatives := self hookBranchOf: regexNode onto: endNode.
	^(RxmMarker new index: startIndex)
		pointTailTo: alternatives;
		yourself