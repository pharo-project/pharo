user interface
choosePackagesIn: aWindow 
	| result list |
	list := MorphTreeModel new
				beCheckList;
				autoMultiSelection: true;
				rootItems: (self allPackagesWithSettings asArray sort: [:a :b | a packageName < b packageName]);
				 wrapBlockOrSelector: #packageName.
	result := list openDialogWindowIn: aWindow title: 'Choose packages' selectedtems: self viewedPackages.
	result ifNotNil: [self changePackageSet: result]