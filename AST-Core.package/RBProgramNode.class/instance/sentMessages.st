accessing
sentMessages
	^ self children
		inject: Set new
		into: [ :messages :each | 
			messages
				addAll: each sentMessages;
				yourself ]