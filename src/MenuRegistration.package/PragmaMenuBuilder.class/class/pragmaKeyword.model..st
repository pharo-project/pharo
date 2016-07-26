instance creation
pragmaKeyword: aPragmaKeyword model: aModel
	"Build a builder using aPragmaKeyword as the pragma keyword and aModel a the model of the resulting builder"
	^ self withAllPragmaKeywords: (Array with: aPragmaKeyword) model: aModel