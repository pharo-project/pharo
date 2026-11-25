Extension { #name : 'MenuMorph' }

{ #category : '*Morphic-Halos' }
MenuMorph >> wantsMetaMenu [
	"Only menus that stay up get a meta menu"

	^ stayUp
]
