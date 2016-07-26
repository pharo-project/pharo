delivery
deliver: aGTEventMultiBundle signalProgress: aBoolean
	^ (ZnClient new
			beOneShot;
			timeout: 5;
			signalProgress: aBoolean;
			numberOfRetries: 0;
			url: aGTEventMultiBundle url;
			enforceHttpSuccess: true;
			ifFail: [ :exception | queue nextPut: aGTEventMultiBundle ];
			entity: aGTEventMultiBundle entity;
			put;
			response)