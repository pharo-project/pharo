history
package: aPackage class: aClass protocol: aProtocol method: aMethod

	self 
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedCategory: aProtocol;
		selectedMethod: aMethod.
		
	self announcer announce: (NautilusMethodSelected
										package: aPackage
										class: aClass
										protocol: aProtocol
										method: aMethod).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusMethodSelected ].
	self ui ifNotNil: [ self ui resetIconStyle ].
	
	self registerHistoryNewEntry