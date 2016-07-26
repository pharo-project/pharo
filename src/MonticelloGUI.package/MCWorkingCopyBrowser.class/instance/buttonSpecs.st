menu spec
buttonSpecs
	^ #(#('+Package' #addWorkingCopy 'Add a new package and make it the working copy')
		#('+Config' #addMetacelloConfiguration 'Add a new metacello configuration')
		#('+Slice' #addSlice 'Add a slice to cleaning and nicely propose a bug fix')
		#('Browse' #browseWorkingCopy 'Browse the working copy of the selected package' #hasWorkingCopy)
		#('Changes' #viewChanges 'View the working copy''s changes relative to the installed version from the repository' #canSave)
	 	#('+Repository' #addRepository 'Add an existing repository to the list of those visible')
		#('Save' #saveVersion 'Save the working copy as a new version to the selected repository' #canSave)
		#('Open' #openRepository 'Open a browser on the selected repository' #hasRepository))