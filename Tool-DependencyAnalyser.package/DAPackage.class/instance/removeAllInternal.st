dependencies
removeAllInternal
	dependencies := dependencies reject: [ :each | each isInternal ]