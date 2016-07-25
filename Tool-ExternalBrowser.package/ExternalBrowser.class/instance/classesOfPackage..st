structure accessing
classesOfPackage: aPackage
	| theClasses |
	theClasses := aPackage classes.
	^ self showClassSide state
		ifTrue: [ theClasses collect: #theMetaClass ]
		ifFalse: [ theClasses ]