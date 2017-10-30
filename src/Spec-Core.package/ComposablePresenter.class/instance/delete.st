private
delete

	window value
		ifNil: [ self changed: #delete with: #() ]
		ifNotNil: [ :w | w delete ]