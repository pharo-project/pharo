*Deprecated60
keyText: aString
	"I set the shortcut for this action based on the first character of the given string."
	self
		deprecated: 'Use #keymap: instead to set a keymap object as shortcut.'
		on: '2016-09-05'
		in: #Pharo6.
	
	aString ifNil: [ 
		self keymap: nil.
		^ self ].
	keymap := 
		(aString first isUppercase 
			ifTrue: [ aString first meta shift ]
			ifFalse: [ aString first meta ])