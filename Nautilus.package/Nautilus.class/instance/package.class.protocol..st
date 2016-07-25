history
package: aPackage class: aClass protocol: aProtocol

	self 
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedCategory: aProtocol.
		
	self announcer announce: (NautilusProtocolSelected
										package: aPackage
										class: aClass
										protocol: aProtocol).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusProtocolSelected ].
	
	self registerHistoryNewEntry