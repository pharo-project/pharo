running
check: aClass forCritiquesDo: aCritiqueBlock
	((aClass inheritsFrom: Collection)
	and: [ aClass isVariable 
	and: [ (aClass includesSelector: #copyEmpty) not 
	and: [ aClass instVarNames isEmpty not ]]]) ifTrue: [
		aCritiqueBlock cull: (self critiqueFor: aClass) ]