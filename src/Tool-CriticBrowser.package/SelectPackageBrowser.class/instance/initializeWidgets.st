initialization
initializeWidgets

	super initializeWidgets.	
	addTestCaseClassesModel := self newCheckBox.
	removeTestPackageModel := self newButton.
	self setRemoveTestPackageModel.		
	addTestCaseClassesModel label: 'remove all TestCase Classes'.
	addTestCaseClassesModel state: true.