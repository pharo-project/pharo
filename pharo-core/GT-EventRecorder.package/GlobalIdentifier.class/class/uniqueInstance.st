accessing
uniqueInstance
	"self uniqueInstance"
	uniqueInstance ifNil: [ self initializeUniqueInstance ].
	^ uniqueInstance