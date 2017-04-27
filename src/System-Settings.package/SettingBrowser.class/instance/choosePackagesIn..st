user interface
choosePackagesIn: aWindow 
	"Method called when one presses the 'Choose packages' buttong.
	This button restrict the set of setting to be considered. By selecting one or more packages, only the setting of these packages are displayed."
	| result list |
	list := MorphTreeModel new
				beCheckList;
				autoMultiSelection: true;
				rootItems: (self allPackagesWithSettings asArray sort: [:a :b | a packageName < b packageName]);
				 wrapBlockOrSelector: #packageName.
	result := list openDialogWindowIn: aWindow title: 'Choose packages' selectedtems: self viewedPackages.
	result ifNotNil: [self changePackageSet: result]