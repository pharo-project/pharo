filtering
contents
	^ contents ifNil: [ 
		self buildChildren.
		contents ]