running
basicCheck: aMethod

	^ (self lintCheckSelectors includes: aMethod selector) and: [
	  
	  (aMethod methodClass inheritsFrom: RBLintRule)       and: [ 
		
		aMethod selfMessages noneSatisfy: [ :message |
			self assert: message isSymbol.
			message = #basicCheck: ] ] ]
	