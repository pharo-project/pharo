accessing
removeInfoKey: key
	^ self removeInfoKey: key ifAbsent: [ "ignore" ]