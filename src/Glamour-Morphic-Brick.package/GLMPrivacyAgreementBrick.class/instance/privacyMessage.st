accessing
privacyMessage

	^ self sendUsageData 
		ifTrue: [ 'You are sending diagnostic and usage data to help us improve Pharo. Thanks!' ]
		ifFalse: 	['No diagnostic and usage data is being sent. Would you like to send diagnostic and usage data to help us improve Pharo?']