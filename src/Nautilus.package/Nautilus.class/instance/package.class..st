history
package: aPackage class: aClass

	self 
		selectedPackage: aPackage;
		selectedClass: aClass.
		
	self announcer announce: (NautilusClassSelected package: aPackage class: aClass).
	self ui ifNotNil: [ self ui sourceTextModel announce: NautilusClassSelected ].
	
	self registerHistoryNewEntry