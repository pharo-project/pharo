private
hookBranchOf: regexNode onto: endMarker
	"Private - Recurse down the chain of regexes starting at
	regexNode, compiling their branches and hooking their tails 
	to the endMarker node."

	| rest |
	rest := regexNode regex isNil
		ifTrue: [nil]
		ifFalse: [self hookBranchOf: regexNode regex onto: endMarker].
	^RxmBranch new
		next: ((regexNode branch dispatchTo: self)
					pointTailTo: endMarker; 
					yourself);
		alternative: rest;
		yourself