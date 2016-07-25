accessing
showHierarchy: aBoolean
	self packageWidget enabled: aBoolean not.
	aBoolean
		ifTrue: [ self basicShowHierarchy ].
	self resetClassesList.
	self classWidget showHierarchy: aBoolean.
	"If showPackages false(Hierarchy)-->true(Flat) then selected package needs to synch to that of selected class"
	(aBoolean not and: [ self model showHierarchy and: [ self selectedClass notNil ]])
		ifTrue: [ 
			(self selectedPackage includesClass: self selectedClass class)
				ifFalse: [ self selectPackageFor: self selectedClass ] ]. 
	"We check if we had a selectedMethod before toggling hierarchy button"
	self model showHierarchy: aBoolean.
	self updateClassView.
	self changed: #hierarchyButtonState