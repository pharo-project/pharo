private
standardizeDevVersionString: aDevVersionString
	| versionString |
	
	versionString := aDevVersionString copyReplaceAll: 'baseline-' with: ''.
	versionString  :=  versionString copyReplaceAll: '-baseline' with: ''.
	versionString  :=  versionString copyReplaceAll: 'baseline' with: ''.
	^ versionString , '-baseline'