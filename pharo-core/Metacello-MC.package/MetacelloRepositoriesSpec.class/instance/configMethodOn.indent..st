printing
configMethodOn: aStream indent: indent

	aStream 
		tab: indent; 
		nextPutAll: 'spec';
		cr.
	self configMethodCascadeOn: aStream indent: indent