tests
testDefault
	(GLMLoggedObject withAllSubclasses reject: [:each | 
		each hasAbstractMethods ]) do: [ :each |
			self assert: each new logger == GLMLogger nullInstance ]