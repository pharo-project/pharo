accessing
processors

	^ processors ifNil: [ 
		processors := OrderedCollection new.
		self origin spotterProcessorsFor: self.
		"this is a hack. we do it here just because we need to store the
		category name in the brick after the processors have been created"
		processors do: [ :each | 				
			self candidates addProcessor: each ].
		processors ]