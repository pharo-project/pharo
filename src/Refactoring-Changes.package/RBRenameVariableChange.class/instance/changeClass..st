accessing
changeClass: aBehavior
	isMeta := aBehavior isMeta.
	className := isMeta
		ifTrue: [ aBehavior soleInstance name ]
		ifFalse: [ aBehavior name ]