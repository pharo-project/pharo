api
taskbarIcon

	^ model value 
		ifNil: [ super taskbarIcon ]
		ifNotNil: [ :m |
			m windowIcon
				ifNil: [ m taskbarIcon ] ]