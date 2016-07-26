initialization
initialize
	| wrappingBlock selectedRulesChildrenBlock rulesToSelectChildrenBlock |
	super initialize.
	toSelectModel := self instantiate: TreeModel.
	selectedModel := self instantiate: TreeModel.
	selectedRules := Set new.
	selectedRulesChildrenBlock := [ :rule | 
		rule isComposite
			ifTrue: [ rule rules
				select: [ :r | (self isSelected: r) or: [ r isComposite ] ] ]
			ifFalse: [ #() ] ].
	rulesToSelectChildrenBlock := [ :rule | 
		rule isComposite
			ifTrue: [ rule rules
				select: [ :r | 	(self isSelected: r) not or: [ r isComposite ] ] ]
			ifFalse: [ #() ] ].
	toSelectModel childrenBlock: rulesToSelectChildrenBlock.
	selectedModel childrenBlock: selectedRulesChildrenBlock.
	wrappingBlock := [ :rule | rule name ].
	toSelectModel displayBlock: wrappingBlock.
	selectedModel displayBlock: wrappingBlock