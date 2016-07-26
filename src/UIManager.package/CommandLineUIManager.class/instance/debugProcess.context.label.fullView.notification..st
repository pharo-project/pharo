debug
debugProcess: process context: context label: title fullView: bool notification: notificationString
	| description |
	description := String streamContents: [ :stream |
		stream
			nextPutAll: 'debugging process: ';
			nextPutAll: process name;
			cr;
			nextPutAll: 'context:';
			cr;
			print: context.
		notificationString ifNotNil: [ stream nextPutAll: notificationString ] ].
	self 
		logTitle: title
		andDescription: description 
		to: self stderr