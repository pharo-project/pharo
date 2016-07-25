t - accessing
rootNodeClassFromItem: anItem 
	"To have the good class for my nodes, I ask my owner,
	because he is the only one who knows his state"
	^ self 
		forSelectorsDo: [
			(self resultDictionary at: anItem) size > 1
				ifTrue: [ FinderMethodNode ]
				ifFalse: [ FinderSingleMethodNode ]]
		forClassNamesDo: [FinderClassNode]
		forSourceDo: [FinderMethodNode]
		forExamplesDo: [FinderExampleMethodNode] 
		forPragmasDo: [FinderPragmaNode]