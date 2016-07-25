actions
addPackageRequirement
	"Add a requirement (a group, another package) for the selected package."
	
	packagesList selectedItem
		ifNil: [ UIManager default inform: 'No package selected' ]
		ifNotNil: [ :packageNode |
			|  potentialRequirements |
			potentialRequirements := project dependencies 
											reject: [ :item | | selectedPackage |
													selectedPackage := packageNode content.
													(item = selectedPackage) or: [ selectedPackage dependencies includes: item ] ]
											thenCollect: [ :dependency | dependency name ].
			^ UIManager default chooseFrom: potentialRequirements values: potentialRequirements ].
	^ nil