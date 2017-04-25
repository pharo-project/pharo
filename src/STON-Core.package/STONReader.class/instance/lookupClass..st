private
lookupClass: name
	Smalltalk globals 
		at: name 
		ifPresent: [ :class | ^ class ].
	^ classes 
		at: name 
		ifAbsentPut: [
			Object allSubclasses 
				detect: [ :class | class isMeta not and: [ class stonName = name ]  ]
				ifNone: [ NotFound signalFor: name ] ]
	