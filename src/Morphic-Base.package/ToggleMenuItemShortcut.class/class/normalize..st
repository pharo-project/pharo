private
normalize: aString
	"This is a hack to show better explained menu commands. 
	 WARNING: This is now really good, but is an easy and fast way to enhance accesibility of 
	 menu commands."
	
	self keyTextShortcuts do: [ :each |
		(aString includesSubstring: each caseSensitive: false)
			ifTrue: [ ^ self normalizeFullStyle: aString ] ].   
	^ self normalizeCondensedStyle: aString