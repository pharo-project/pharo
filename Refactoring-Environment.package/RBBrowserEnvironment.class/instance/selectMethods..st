environments
selectMethods: aBlock 
	| env |
	env := RBSelectorEnvironment onEnvironment: self.
	self classesAndSelectorsDo: 
			[:each :sel | 
			(aBlock value: (each compiledMethodAt: sel)) 
				ifTrue: [env addClass: each selector: sel]].
	^env