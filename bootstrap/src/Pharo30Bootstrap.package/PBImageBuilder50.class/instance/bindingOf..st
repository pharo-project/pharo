accessing instances and variables
bindingOf: aName

	(self includesClassNamed: aName)
		ifTrue: [ ^ EPGlobalBinding new
				name: aName objectSpace: self;
				yourself ].
			
	^ nil