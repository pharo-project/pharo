accessing
showHierarchy: aBoolean
	|selectedClass|
	
	(selectedClass := self selectedClass) ifNil: [ ^ self ]. "Since the double clic comes after a simple clic and simple clic deselects selected item, we are forced to have this guard. A alternative would be to change the simple clic but the side effects are too big"
	self resetClassesListSelection.
	self resetClassesList.
	self classWidget showHierarchy: aBoolean forClass: selectedClass theNonMetaClass.

	"If showPackages false(Hierarchy)-->true(Flat) then selected package needs to synch to that of selected class"
	self packageWidget enabled: aBoolean not.
	(aBoolean not and: [ self model showHierarchy and: [ self selectedClass notNil ]])
		ifTrue: [ 
			(self selectedPackage includesClass: self selectedClass class)
				ifFalse: [ self selectPackageFor: self selectedClass ] ]. 
	"We check if we had a selectedMethod before toggling hierarchy button"
	self model showHierarchy: aBoolean.
	
	self updateClassView.
	self classWidget selectClass: self selectedClass.
	self updateClassView.	
	self changed: #hierarchyButtonState