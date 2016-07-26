private
storeReference: object
	| index |
	index := objects size + 1.
	objects at: index put: object.
	^ index