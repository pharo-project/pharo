accessing
children
	^ [ self content dependencies collect: [ :each | 'loads: ' , each] ]