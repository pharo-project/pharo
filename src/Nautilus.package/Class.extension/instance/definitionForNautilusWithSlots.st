*Nautilus
definitionForNautilusWithSlots
	"the class definition with a way to specify slots. Shown when the class defines special Slot
	 NOTE: this is just for testing, the final solution will evolve and be cleaner later"

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."


	| aStream poolString|

	poolString := self sharedPoolsString.

	aStream := (String new: 800) writeStream.
	superclass 
		ifNil: [aStream nextPutAll: 'ProtoObject']
		ifNotNil: [aStream nextPutAll: superclass name].
	aStream nextPutAll: ' subclass: ';
			store: self name.
	(self hasTraitComposition) ifTrue: [
		aStream cr; tab; nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString].		
			
	(self classLayout isKindOf: FixedLayout) ifFalse: [
		aStream cr; tab; nextPutAll: 'layout: ';
			nextPutAll: self classLayout class name].
	aStream cr; tab; nextPutAll: 'slots: ';
			nextPutAll: self slotDefinitionString.
	aStream cr; tab; nextPutAll: 'classVariables: ';
			nextPutAll: self classVariableDefinitionString.
	poolString = '' ifFalse: [
		aStream cr; tab; nextPutAll: 'poolDictionaries: ';
			store: poolString].
	aStream cr; tab; nextPutAll: 'category: ';
			store: self category asString.

	superclass ifNil: [ 
		aStream nextPutAll: '.'; cr.
		aStream nextPutAll: self name.
		aStream space; nextPutAll: 'superclass: nil'. ].

	^ aStream contents