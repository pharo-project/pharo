instance creation
instanceLike: aDefinition
	"The cache is playing havoc with the equality between methods. Methods of the same code but with different timestamps are considered equal. This breaks havoc with some filetree testing code which looks at timestamps."

	| aMCMethodDefinition |
	aMCMethodDefinition := super instanceLike: aDefinition.
	^ aMCMethodDefinition timeStamp ~= aDefinition timeStamp
		ifTrue: [ Instances add: aDefinition ]
		ifFalse: [ aMCMethodDefinition ]