private dispatch
removeClassCategory: packageTag 
	self basicRemoveClasses: packageTag classes.
	packageTag asNautilusSelection item removeFromPackage.