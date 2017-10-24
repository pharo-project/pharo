*Spec-MorphicAdapters
adoptMenuGroupModel: aGroupModel first: aBoolean
	| controls |
	
	aBoolean ifFalse: [ 
		self 
			addMorphBack: (Smalltalk ui theme newToolSpacerIn: self);
			addMorphBack: (Smalltalk ui theme newToolbarHandleIn: self);
			addMorphBack: (Smalltalk ui theme newToolSpacerIn: self) ].
			
	controls := OrderedCollection new.
	aGroupModel menuItems do: [ :item | self adoptMenuItemModel: item accumulator: controls ].
	
	self emptyAccumulator: controls