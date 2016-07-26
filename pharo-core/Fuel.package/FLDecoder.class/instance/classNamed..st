accessing
classNamed: className 

	^ (migrations
		detect: [:m | m sourceClassName = className ]
		ifNone: [ ^ self globalClassNamed: className ])
		targetClass.
		