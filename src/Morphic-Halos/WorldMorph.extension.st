Extension { #name : 'WorldMorph' }

{ #category : '*Morphic-Halos' }
WorldMorph >> addStandardHaloMenuItemsTo: aMenu hand: aHandMorph [
	"Add standard halo items to the menu"

	self addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
]

{ #category : '*Morphic-Halos' }
WorldMorph >> buildMetaMenu: evt [
	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	self addStandardHaloMenuItemsTo: menu hand: evt hand.
	^ menu
]

{ #category : '*Morphic-Halos' }
WorldMorph >> haloMorphs [

	^ self hands collect: [ :h | h halo ] thenSelect: [ :halo | halo isNotNil ]
]

{ #category : '*Morphic-Halos' }
WorldMorph >> wantsDirectionHandles [

	^ false
]
