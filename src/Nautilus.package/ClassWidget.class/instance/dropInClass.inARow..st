private list model
dropInClass: aCollection inARow: aRow
	| receiver |
	aRow>0 ifTrue: [  
		receiver := self getClassesList at: aRow.
		receiver := 
		self model showInstance not
			ifTrue: [ receiver theMetaClass ]
			ifFalse: [ receiver theNonMetaClass ].
		self model dropInAClass: aCollection into: receiver]