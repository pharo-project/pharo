menus behavior
compareTwoMethods
	
	(DiffPresenter new
		showOptions: true;
		leftText: self selectedMethods first sourceCode;
		rightText: self selectedMethods second sourceCode;
		contextClass: self selectedMethods first methodClass;
		openWithSpec) title: 'Compare methods in ', self selectedMethods first methodClass name
	
	