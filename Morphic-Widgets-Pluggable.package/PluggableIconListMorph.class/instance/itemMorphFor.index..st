display
itemMorphFor: anObject index: anIndex
	"Answer a morph for the object with the appropriate icon."

	|item icon|
	item := IconicListItem new
		originalObject: anObject;
		yourself.
		
	icon := self getIconSelector ifNotNil: [self model perform: self getIconSelector withEnoughArguments: {anObject. anIndex}].
	icon ifNotNil: [
		item icon: icon asMorph].
	item morph: (self wrapItem: anObject index: anIndex) asMorph.
	^item