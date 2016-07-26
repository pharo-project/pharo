printing
configMethodOn: aStream indent: indent

	aStream 
		tab: indent; 
		nextPutAll: 'spec '.
	self configMethodCascadeOn: aStream lastCascade: true