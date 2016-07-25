*Nautilus
definitionForNautilus
	"Answer a String that defines the receiver."

	| aStream poolString |
	((self usesSpecialSlot 
		or: [ self usesSpecialClassVariables ])
		or: [ Slot showSlotClassDefinition ])
		ifTrue: [ ^ self definitionForNautilusWithSlots ].
	poolString := self sharedPoolsString.
	aStream := (String new: 800) writeStream.
	superclass
		ifNil: [ aStream nextPutAll: 'ProtoObject' ]
		ifNotNil: [ aStream nextPutAll: superclass name ].
	aStream
		nextPutAll: self kindOfSubclass;
		store: self name.
	self hasTraitComposition
		ifTrue:
			[ aStream
				cr;
				tab;
				nextPutAll: 'uses: ';
				nextPutAll: self traitCompositionString ].
	aStream
		cr;
		tab;
		nextPutAll: 'instanceVariableNames: ';
		store: self instanceVariablesString.
	aStream
		cr;
		tab;
		nextPutAll: 'classVariableNames: ';
		store: self classVariablesString.
	poolString = ''
		ifFalse:
			[ aStream
				cr;
				tab;
				nextPutAll: 'poolDictionaries: ';
				store: poolString ].
	aStream
		cr;
		tab;
		nextPutAll: 'package: ';
		store: self category asString.
	superclass
		ifNil:
			[ aStream
				nextPutAll: '.';
				cr.
			aStream nextPutAll: self name.
			aStream
				space;
				nextPutAll: 'superclass: nil' ].
	^ aStream contents