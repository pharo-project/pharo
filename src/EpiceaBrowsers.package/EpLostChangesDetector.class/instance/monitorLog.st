accessing
monitorLog

	^ monitorLog ifNil: [ monitorLog := EpMonitor current log ]