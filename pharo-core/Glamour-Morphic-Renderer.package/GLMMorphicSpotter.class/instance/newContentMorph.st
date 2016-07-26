morphic
newContentMorph
	^ (self newColumn: 
			((OrderedCollection with: self newTextMorph) addAll: self newListMorphs; yourself) )
		minWidth: 320;
		minHeight: 200;
		yourself