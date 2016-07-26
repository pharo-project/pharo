public
textLowercase
	^ textLowercase ifNil: [ textLowercase := self defaultTextLowercase ]