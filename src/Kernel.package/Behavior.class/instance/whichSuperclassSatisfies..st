queries
whichSuperclassSatisfies: aBlock 
	(aBlock value: self) ifTrue: [^self].
	^self superclass ifNotNil: [self superclass whichSuperclassSatisfies: aBlock]