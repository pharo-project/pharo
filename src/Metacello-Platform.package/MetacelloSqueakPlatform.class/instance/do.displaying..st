notification
do: aBlock displaying: aString

	self bypassProgressBars ifTrue: [ ^super do: aBlock displaying: aString ].
	aString
		displayProgressAt: Sensor cursorPoint
		from: 0 to: 2
		during: [:bar |
			bar value: 1.
			aBlock value.
			bar value: 2 ]