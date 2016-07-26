building
build
	"self new build openInWorld"
	
	| window |
	window := (SystemWindow labelled: self label) model: self; extent: self extent.
	
	window addMorph: self buildPackageSearch fullFrame: self packageSearchFrame.
	window addMorph: self buildPackages fullFrame: self packagesFrame.
	
	window addMorph: self buildClassesSearch fullFrame: self classesSearchFrame.
	window addMorph: self buildClasses fullFrame: self classesFrame.
	
	window	addMorph: self buildStatus fullFrame: self statusFrame. 
	window	addMorph: self buildFailures fullFrame: self failureListFrame. 
	window	addMorph: self buildErrors fullFrame: self errorListFrame. 
	window	addMorph: self buildButtons fullFrame: self buttonsFrame. 
	^ window
	
	
	