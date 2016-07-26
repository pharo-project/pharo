actions
addDeclaredPackagesUnderTestTo: listOfPackages 
	classesSelected do: 
		[ :class | 
		(class class includesSelector: #packageNamesUnderTest) ifTrue: 
			[ class packageNamesUnderTest do: [ :name | listOfPackages add: (RPackage organizer packageNamed: name) ] ] ]