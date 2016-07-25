menu
canCompareToCurrent

	^ changes selectedItems size = 1
		and: [ changes selectedItem isMethodDeclaration ]