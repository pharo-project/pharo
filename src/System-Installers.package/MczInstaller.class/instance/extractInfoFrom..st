utilities
extractInfoFrom: dict
	dict at: #id put: (UUID fromString: (dict at: #id)).
	dict at: #date ifPresent: [:d | d isEmpty ifFalse: [dict at: #date put: (Date fromString: d)]].
	dict at: #time ifPresent: [:t | t isEmpty ifFalse: [dict at: #time put: (Time readFrom: t readStream)]].
	dict at: #ancestors ifPresent: [:a | dict at: #ancestors put: (a collect: [:ea | self extractInfoFrom: ea])].
	^ dict