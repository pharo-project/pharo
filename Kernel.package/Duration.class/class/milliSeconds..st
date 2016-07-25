instance creation simple
milliSeconds: milliCount
     
	^ self
		seconds: (milliCount quo: 1000)
		nanoSeconds: (milliCount rem: 1000) * NanosInMillisecond