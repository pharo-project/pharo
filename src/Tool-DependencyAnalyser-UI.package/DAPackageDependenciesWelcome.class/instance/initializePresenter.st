initialization
initializePresenter
	self actionOnOpen;
		actionOnAddPackage;
		actionOnRemovePackage.
	
	self buttonAddAll action: [ self packageChosenList items: self packageList listItems. self updatePackageChosenLabel ].
	self buttonRemoveAll action: [ self packageChosenList items: #().  self updatePackageChosenLabel ].
		
	packageChosenList whenSelectedItemChanged: [ packageList resetSelection ].
	
	textPackageField
		whenTextChanged: [ self filterPackageList ]