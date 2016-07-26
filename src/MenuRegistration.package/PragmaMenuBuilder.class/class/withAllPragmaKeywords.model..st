instance creation
withAllPragmaKeywords: aCollection model: aModel
	"Build a builder using aPragmaKeyword as the pragma keyword and aModel a the model of the resulting builder"
	^ self new 
		pragmaKeywords: aCollection;
		model: aModel;
		yourself.