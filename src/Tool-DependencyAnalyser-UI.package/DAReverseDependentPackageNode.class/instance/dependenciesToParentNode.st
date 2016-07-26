private
dependenciesToParentNode
	^ self content daPackage 
		compositeDependencyTo: (self parentNode content daPackage)