textconstants access
fontArrayForStyle: aName
	"Answer the fonts in the style named aName,
	or an empty Array if no such named style."

	"TextStyle fontArrayForStyle: #Atlanta"
	"TextStyle fontPointSizesFor: 'NewYork'"

	^ ((self named: aName) ifNil: [ ^#() ]) fontArray
