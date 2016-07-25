private
targetFor: varName in: aClass
	^ varName first isUppercase 
		ifTrue: [ aClass class ] ifFalse: [aClass].
