testing-environments
testClassEnvironment
	| aClassEnvironment |
	aClassEnvironment := RBClassEnvironment 
		onEnvironment: universalEnvironment 
		classes: (Array with: Object with: Object class).
	self universalTestFor: aClassEnvironment.
	self deny: (universalEnvironment isClassEnvironment).
	self assert: (aClassEnvironment isClassEnvironment).
	self assert: (aClassEnvironment packages size) equals: 1.
	self assert: (aClassEnvironment implementorsOf: #printString) numberSelectors equals: 1.
	
	self assert: (RBClassEnvironment class: Class) selectors size 
    	  equals: Class methods size.
	self assert: (RBClassEnvironment class: TBehavior) selectors size
    	  equals: TBehavior methods size.