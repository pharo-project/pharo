accessing
keymap

	^ keymap ifNil: [ 
		self defaultKeymap ifNil: [ 
			"Code to ensure backwards compatibility with the previous
			solution that used #defaultKeyText."
			self defaultKeyText ifNotNil: [ :aString |
				aString first isUppercase 
					ifTrue: [ aString first meta shift ]
					ifFalse: [ aString first meta ] ] ] ]