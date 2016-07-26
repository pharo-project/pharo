running
basicCheck: aClass 
	
	^ (aClass instVarNames anySatisfy:  
		[ :each | 
		aClass withAllSubclasses 
			noneSatisfy:  [ :class |
				(class whichSelectorsAccess: each) isNotEmpty ] ]) or: [  
		aClass isMeta not and: 
			[ aClass classPool associations anySatisfy: [ :each | 
				(SystemNavigation default allCallsOn: each) isEmpty ] ] ]