*Versionner-Spec-Browser
"protocol: *Versionner-Spec-Browser"
mcPackageSearchRejectAll: aRejectBlock withInitialFilter: filter
	| workingCopies |
	
	workingCopies := MCWorkingCopy allManagers asSortedCollection: [ :a :b | a packageName <= b packageName].
	workingCopies := workingCopies reject: aRejectBlock.
	
	^ ListDialogWindow new
				getList: [ :regex | workingCopies select: [ :workingCopy | regex search: workingCopy package name] ];
				displayBlock: [ :workingCopy | workingCopy package name ];
				title: 'Monticello packages';
				initialFilter: (filter ifNil: [ '' ] ifNotNil: [ filter]);
				yourself. 
