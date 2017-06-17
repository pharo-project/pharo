tests
testDefault
	(GLMLoggedObject withAllSubclasses 
		reject: [:each | 
			each allMethods anySatisfy: [ :cm | cm sendsSelector: #subclassResponsibility ] ])
		do: [ :each |
			self assert: each new logger == GLMLogger nullInstance ]