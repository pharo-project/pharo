accessing
enabled
	"Answer whether the receiver is enabled"
	^ enabledBlock
		ifNil:[enabled ifNil: [ true ]]
		ifNotNil: [ enabledBlock value ]