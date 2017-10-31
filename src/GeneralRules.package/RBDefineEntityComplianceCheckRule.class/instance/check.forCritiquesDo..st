running
check: aClass forCritiquesDo: aCritiqueBlock
	
	((aClass inheritsFrom: RBLintRule) and: [ 
		aClass isVisible and: [
			self complianceMethods noneSatisfy: [ :method |
				aClass perform: method ] ] ]) ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aClass) ]