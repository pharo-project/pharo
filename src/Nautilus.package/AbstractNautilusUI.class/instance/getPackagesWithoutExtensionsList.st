private
getPackagesWithoutExtensionsList
	
	^ self model packagesWithoutExtensions sort: [:a :b | a name <= b name ]