accessing
classes
	^ self classNames collect: [ :each | self class environment at: each ]