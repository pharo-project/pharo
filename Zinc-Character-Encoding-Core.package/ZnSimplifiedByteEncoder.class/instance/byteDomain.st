accessing
byteDomain
	"Return an array with the byte values that are in my domain, that I can decode"
	
	^ (0 to: 127) asArray , ((byteToUnicode withIndexCollect: [ :each :index | each ifNotNil: [ index + 127 ] ]) reject: [ :each | each isNil ]) 