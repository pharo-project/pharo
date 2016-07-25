accessing
layoutClass
	"If not specified the layout class is the same as the superclass"
	^ layoutClass ifNil: [ layoutClass := superclass classLayout class ]