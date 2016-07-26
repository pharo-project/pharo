accessing
reset
	"Stop and remove the process for sending usage data. Delete all recorder that from the queue."
	uniqueInstance ifNil: [ ^ self ].
	SystemAnnouncer uniqueInstance unsubscribe: uniqueInstance.
	uniqueInstance deactivateDelivery ifNotNil: [ :aProcess | 
			[ aProcess terminate ] on: Error do: [ "ignore error" ] ].
	uniqueInstance := nil.