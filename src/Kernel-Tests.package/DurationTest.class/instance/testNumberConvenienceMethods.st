tests
testNumberConvenienceMethods

	self
		assert: 1 week = (Duration days: 7);
		assert: -1 week = (Duration days: -7);
		assert: 1 day = (Duration days: 1);
		assert: -1 day = (Duration days: -1);
		assert: 1 hours = (Duration hours: 1);
		assert: -1 hour = (Duration hours: -1);
		assert: 1 minute = (Duration seconds: 60);
		assert: -1 minute = (Duration seconds: -60);
		assert: 1 second = (Duration seconds: 1);
		assert: -1 second = (Duration seconds: -1);
		assert: 1 milliSecond = (Duration milliSeconds: 1);
		assert: -1 milliSecond = (Duration milliSeconds: -1);
		assert: 1 nanoSecond = (Duration nanoSeconds: 1);
		assert: -1 nanoSecond = (Duration nanoSeconds: -1)
		