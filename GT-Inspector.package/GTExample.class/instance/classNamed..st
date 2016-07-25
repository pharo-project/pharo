private
classNamed: aClassOrClassName
	^ aClassOrClassName isClass 
		ifTrue: [ aClassOrClassName ]
		ifFalse: [ Smalltalk classNamed: aClassOrClassName asString ]