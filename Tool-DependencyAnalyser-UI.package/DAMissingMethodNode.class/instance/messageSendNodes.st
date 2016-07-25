api
messageSendNodes
	^ self messageSendAnalysis rPackage methods
		select: [ :method | method sendsSelector: self content ]
		thenCollect: [ :method | 
			DAMessageSendNode new
				content: method;
				parentNode: self;
				yourself ]