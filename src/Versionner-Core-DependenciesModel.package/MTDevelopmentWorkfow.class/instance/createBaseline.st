private
createBaseline
	"Create a new baseline with information given by the project" 
	|lastBaseline baselineVersionString|
	
	lastBaseline := self configurationClass baselines detect: [:aBaseline | aBaseline isBleedingEdge].
	baselineVersionString := (lastBaseline name asInteger +1) asString.
	
	^ self createBaseline: baselineVersionString.