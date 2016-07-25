running
checkClass: aClass 
	(aClass whichSelectorsReferTo: #subclassResponsibility) do: 
		[ :each | 
		(aClass subclasses 
			anySatisfy:  
				[ :class | 
				class subclasses isEmpty and:
				[ (class whichClassIncludesSelector: each) == aClass ] ])  ifTrue: 
			[ result 
				addClass: aClass
				selector: each ] ]