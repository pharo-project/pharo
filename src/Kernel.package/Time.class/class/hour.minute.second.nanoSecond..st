instance creation
hour: hour minute: minute second: second  nanoSecond: nanoCount
	"Answer a Time - only second precision for now"
 
 	^ self 
		seconds: (hour * SecondsInHour) + (minute * SecondsInMinute) + second 
		nanoSeconds: nanoCount