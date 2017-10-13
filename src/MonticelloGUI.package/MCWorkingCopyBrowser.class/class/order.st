settings
order
	"
	anInteger represents the order of the mcworkingcopies in the MCBrowser. Its value is between 0 and the number of entries in MCFileRepositoryInspector >> orderSpecs
	"
	^ Order ifNil: [ Order := 2 ]