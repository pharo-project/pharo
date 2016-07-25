initialization
rule: aRule

	rule := aRule.
	(rule isNil or: [ rule isComposite ]) 
		ifTrue: [ ^ self ].	
	self criticsOf:  aRule.
	toolbarModel rule: rule.