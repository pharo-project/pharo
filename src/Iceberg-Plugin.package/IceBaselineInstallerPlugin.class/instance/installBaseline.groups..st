private
installBaseline: icePackage groups: anArray
	| packageDir baselineName |
	
	packageDir := icePackage location parent fullName.
	baselineName := icePackage name allButFirst: self class baselinePrefix size.
	Metacello new
		repository: 'gitlocal://', packageDir;
		baseline: baselineName;
		load: anArray