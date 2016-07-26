running
checkPackage: aPackage 
	rules do: [ :each | each checkPackage: aPackage ]