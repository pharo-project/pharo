accessing
username
	^ username ifNil: [ username := self class defaultUsername ]