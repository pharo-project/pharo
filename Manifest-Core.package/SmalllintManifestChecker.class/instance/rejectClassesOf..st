manifest
rejectClassesOf: aPackage
		
	| mb |
	mb := self manifestBuilderOf: aPackage.
	^  mb isNil 
		ifTrue: [{}] 
		ifFalse: [ mb rejectClasses ]
