running
basicCheck: aClass 
	^ (aClass whichSelectorsReferTo: #subclassResponsibility) anySatisfy:  
		[ :each | 
		 aClass subclasses 
			anySatisfy:  [ :class | 
				class subclasses isEmpty and: [ (class whichClassIncludesSelector: each) == aClass ] ] ]