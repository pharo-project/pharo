accessing
location: aUrlString
	| pathSegments |
	(self class isResponsibleFor: aUrlString)
		ifFalse: [ Error signal: 'Please provide a smalltalkhub url' ].
	"extract the smalltalkhub properties from the path part in the given URL"
	pathSegments := aUrlString asZnUrl pathSegments reject: [ :each | each = 'mc' ].
	self owner: pathSegments first.
	self project: pathSegments second.