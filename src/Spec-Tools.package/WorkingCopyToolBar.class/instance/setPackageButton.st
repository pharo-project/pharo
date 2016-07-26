initialization
setPackageButton

	packageButton
		state: false;
		label: '+Package';
		help: 'Add a new package and make it the working copy';
		action: [ self createPackage ]