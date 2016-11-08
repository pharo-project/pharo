initialization
initializeButtons
	buttonBrowseCycles := self newButton
		help: 'Find all the cycles where the package is in the system';
		label: 'Find cycles'.
	buttonDefault := self newButton
		help: 'Back to the default settings';
		label: 'Default settings'.
	buttonAddPackage := self newButton
		help: 'Add package to the current analysis';
		label: 'Add packages';
		icon: (self iconNamed: #addIcon).
	buttonRemovePackage := self newButton
		help: 'Remove package to the current analysis';
		label: 'Remove packages';
		icon: (self iconNamed: #removeIcon).
	buttonGenerateGraphViz := self newButton
		help: 'Open the graph in world';
		label: 'Open the graph'.
	buttonReverseAnalysis := self newButton
		help: 'Reverse the analysis : set the dependent packages as root packages';
		label: 'Reverse the analysis'.
	buttonSave := self newButton
		help: 'Serialize the current object relation graph dependencies';
		label: 'Save current analysis'.
	buttonMakeDiff := self newButton 
		label: 'Make diff'