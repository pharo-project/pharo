test accessing
testExtensionMethods
	"a package can extend several classes, either the class or  the meta-class side. 'extensionMethods' should list all the methods involved in shuch extensions. P3 extend a2 and a2 class"

	"self debug: #testExtendingPackagesOfClass"

	| package extensionMethods cpt |
	package := p3.
	extensionMethods := package extensionMethods.	"self assert: (extensionMethods size = 2)."
	self assert: (extensionMethods includes: a2 >> #methodDefinedInP3).
	self assert: (extensionMethods includes: a2 class >> #classSideMethodDefinedInP3).
	cpt := 0.
	package
		metaclassExtensionSlicesDo: [ :className :listOfSelectors | 
			(className = a2 name and: [ listOfSelectors includes: #classSideMethodDefinedInP3 ])
				ifTrue: [ cpt := cpt + 1 ] ].
	self assert: cpt = 1.
	cpt := 0.
	package
		classExtensionSlicesDo: [ :className :listOfSelectors | 
			(className = a2 name and: [ listOfSelectors includes: #methodDefinedInP3 ])
				ifTrue: [ cpt := cpt + 1 ] ].
	self assert: cpt = 1