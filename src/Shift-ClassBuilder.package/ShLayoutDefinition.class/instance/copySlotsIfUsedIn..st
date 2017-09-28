accessing
copySlotsIfUsedIn: oldClass
	oldClass ifNil:[^self].
	
	oldClass slots isEmpty ifTrue: [ ^self ].
	oldClass classLayout postCopy.		

	oldClass class slots isEmpty ifTrue: [ ^self ].
	oldClass class classLayout postCopy.		
