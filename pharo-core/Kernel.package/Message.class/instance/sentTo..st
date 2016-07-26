sending
sentTo: receiver
	"answer the result of sending this message to receiver"

	^ lookupClass
		ifNil: [ receiver perform: selector withArguments: args]
		ifNotNil: [ receiver perform: selector withArguments: args inSuperclass: lookupClass]