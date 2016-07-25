private
indent: anInteger around: aBlock 
	self indent: self indent + anInteger.
	^aBlock ensure: [self indent: self indent - anInteger]