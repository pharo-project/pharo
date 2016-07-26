accessing
receiverPackage
	^ self settingReceiver
		ifNotNil: [self settingReceiver class package] 