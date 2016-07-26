accessing
urlProvider
	^ urlProvider ifNil: [ urlProvider := 'http://ws.stfx.eu' asZnUrl ]