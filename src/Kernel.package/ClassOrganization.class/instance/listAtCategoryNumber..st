backward compatibility
listAtCategoryNumber: aSmallInteger 
	
	^ (protocolOrganizer allProtocols at: aSmallInteger ifAbsent: [ ^ {} ]) methods asArray