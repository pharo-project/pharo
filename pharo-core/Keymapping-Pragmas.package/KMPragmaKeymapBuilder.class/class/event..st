private
event: anEvent
	anEvent method ifNil: [ ^self ].
	((anEvent method pragmas collect: #keyword) includesAnyOf: self pragmas )
		ifTrue: [ self uniqueInstance reset ]