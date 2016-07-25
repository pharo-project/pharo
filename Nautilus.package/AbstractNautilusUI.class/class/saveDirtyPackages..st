icons behavior
saveDirtyPackages: aCollection 
	
	
	aCollection isEmptyOrNil ifTrue: [ ^ self ].
	aCollection do: [:package || workCopy browser |
		
		workCopy := MCWorkingCopy forPackage: (MCPackage named: package name).
		browser :=MCWorkingCopyBrowser new.
		browser
			show;
			workingCopy: workCopy.
		"wrap := MCDependentsWrapper with: workCopy model: browser.
		self halt.
		index := browser workingCopySelectionWrapper: wrap.
		browser workingCopyTreeMorph selectionIndex: index"]
	