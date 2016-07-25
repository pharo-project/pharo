accessing request
queryAt: key add: value 
	"Modify the receiver by adding a query variable key=value.
	If key already exists, value is added to it."

	request url 
		ifNil: [ request url: (ZnUrl new queryAt: key add: value; yourself) ]
		ifNotNil: [ request url queryAt: key add: value ]  