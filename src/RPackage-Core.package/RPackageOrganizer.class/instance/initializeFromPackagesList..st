initialization-data
initializeFromPackagesList: aPackagesList
 	RPackage 
		withOrganizer: self  
		do:  [  self basicInitializeFromPackagesList: aPackagesList ].