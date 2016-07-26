repository creation
extractTypeFromDescription: description

	(description beginsWith: 'ftp://') ifTrue: [ ^'ftp' ].
	^super extractTypeFromDescription: description