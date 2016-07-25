accessing
calloutAPIClass
	^ CalloutAPIClass ifNil: [ CalloutAPIClass := self defaultCalloutAPIClass ]
	