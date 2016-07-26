browsers
simpleFinder
	<glmBrowser: 'Finder' input: '42'>
	"self new simpleFinder openOn: 42"
	| finder |	
	finder := GLMFinder new.
	finder show: [:a | a list display: [ :x | 1 to: x ]].
	^ finder