public
urlProvider
	^ urlProvider ifNil: [ urlProvider := self class urlProvider ]