showing
show: aReference 
	self pushTraverse: aReference entry.
	self whileNotDoneDo: [ self pop sendTo: self ]