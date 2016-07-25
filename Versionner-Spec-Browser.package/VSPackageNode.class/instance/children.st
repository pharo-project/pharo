accessing
children
	^ [ self content dependencies collect: [ :each | 'requires: ' , each] ]