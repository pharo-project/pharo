accessing
url
	^ url ifNil: [ url := 'http://gc.dcc.uchile.cl.:8080/gt/events' asZnUrl ]