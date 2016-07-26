*GT-InspectorExtensions-Core
localTime
	^ self children
		inject: self time 
		into: [ :diff :each | diff - each time ]