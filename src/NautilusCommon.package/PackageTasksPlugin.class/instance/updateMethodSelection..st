display
updateMethodSelection: aMethod

	model ui categoryWidget
		resetCategoryList;
		resetCategorySelection.

	model ui categoryWidget
		categorySelectionNamed: aMethod category put: true.

	model ui methodWidget 
		resetMethodList;
		resetMethodSelection.
		
	model ui methodWidget 
		selectMethod: aMethod.

	model ui resetClassesListSelection.
	model ui classWidget selectClass: aMethod methodClass.
	model  
		package: model selectedPackage 
		class: aMethod methodClass 
		protocol: aMethod category 
		method: aMethod method.
		
	model ui 
		updateClassView;
		updateCategoryAndMethodList;
		updateOnClassSelection
	