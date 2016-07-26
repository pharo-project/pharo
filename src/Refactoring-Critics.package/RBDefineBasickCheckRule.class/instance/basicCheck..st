running
basicCheck: aClass 
	
	^ (aClass inheritsFrom: RBLintRule) and: [ 
		aClass isVisible and: [ 
	  (aClass lookupSelector: #basicCheck:) isSubclassResponsibility ] ]