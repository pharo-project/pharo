accessing
url
	^ url ifNil: [ url := self defaultUrl ]