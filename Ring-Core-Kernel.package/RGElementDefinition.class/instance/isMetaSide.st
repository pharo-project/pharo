accessing
isMetaSide
	"Even thought several class elements do not define this property (ie. class variables, pool variables) they understand it"
	"This is a derived property from the class definining the receiver and thus its value is kept as an annotation"
	"Default value is false"

	^self annotationNamed: self class isMetaSideKey ifAbsentPut: [ false ]