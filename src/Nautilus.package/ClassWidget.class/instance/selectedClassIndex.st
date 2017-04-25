private list model
selectedClassIndex
	| item |
	item := self selectedClass.
	self model showInstance
		ifFalse: [ item ifNotNil: [ item := item theNonMetaClass ] ].
	^ self getClassesList indexOf: item