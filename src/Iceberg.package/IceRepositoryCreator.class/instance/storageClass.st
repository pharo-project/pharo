accessing
storageClass
	^ storageClass ifNil: [ storageClass := IceRepository defaultBackend ]