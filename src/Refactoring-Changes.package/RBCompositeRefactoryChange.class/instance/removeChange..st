accessing
removeChange: aChange 
	^ changes remove: aChange ifAbsent: [ nil ]