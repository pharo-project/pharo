tests
testMillisecondsToGo
	| delay scheduler millisecondsToGo |
	scheduler := DelayMicrosecondScheduler new.
	delay := Delay forMilliseconds: 100.
	scheduler schedule: delay.
	scheduler handleTimerEvent: Time microsecondClockValue.
	self busyWaitMilliseconds: 50.
	millisecondsToGo := delay millisecondsToGo.
	self assert: millisecondsToGo >= 40.
	self assert: millisecondsToGo <= 60.