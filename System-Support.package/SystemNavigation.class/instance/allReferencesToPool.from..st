query
allReferencesToPool: aPool from: aClass
	"Answer all the references to variables from aPool"

	| list |
	list := OrderedCollection new.
	aClass
		withAllSubclassesDo: [ :cls | 
			cls
				selectorsAndMethodsDo: [ :sel :meth | 
					meth literals
						detect: [ :lit | lit isVariableBinding and: [ (aPool bindingOf: lit key) notNil ] ]
						ifFound: [ list add: (self createMethodNamed: sel realParent: aClass) ] ] ].
	^ list