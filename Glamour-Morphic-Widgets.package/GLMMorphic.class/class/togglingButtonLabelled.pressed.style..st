utils - morph creation
togglingButtonLabelled: anObject pressed: aBoolean style: aSymbol
	| button oldLabel m |
	button := SimpleButtonMorph new.
	(oldLabel := button findA: StringMorph) ifNotNil: [ oldLabel delete ].
	m := self morphElement: anObject.
	button
		addMorph: m;
		borderColor: Color transparent.
	(self isAlignmentMorph: m)
		ifTrue:[ 
			button extent: (m width + 6 ) @ (m height + 6 ).
			m position: 3 @ 3 ]
		ifFalse:[ 
			(self isImageMorph: m)
				ifTrue: [ button extent: (m width + 6) @ (m height + 6) ]
				ifFalse:[ button extent: (m width + 6) @ (m height + 6) ].
			m position: 3 @ 3.	"button center - (m extent // 2)" ].
	self styleButton: button morph: m pressed: aBoolean style: aSymbol.
	m lock.
	^ button