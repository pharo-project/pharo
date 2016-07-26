instance creation
days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: nanos	

 	^ self seconds: ((days * SecondsInDay) 
						+ (hours * SecondsInHour)
							+ (minutes * SecondsInMinute) 
								+ seconds)
		nanoSeconds: nanos
