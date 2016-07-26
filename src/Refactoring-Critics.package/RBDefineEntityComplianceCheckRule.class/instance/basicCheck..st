running
basicCheck: aClass 
	
	^ (aClass inheritsFrom: RBLintRule) and: [ 
		aClass isVisible and: [
			self complianceMethods noneSatisfy: [ :method |
				aClass perform: method ] ] ]