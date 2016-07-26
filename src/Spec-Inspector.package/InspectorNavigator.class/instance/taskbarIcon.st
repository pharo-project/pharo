accessing
taskbarIcon
	^ self inspector 
		ifNil: [ super taskbarIcon ]
		ifNotNil: [ :anInspector | anInspector taskbarIcon ]