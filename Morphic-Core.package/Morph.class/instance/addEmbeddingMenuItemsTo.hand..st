meta-actions
addEmbeddingMenuItemsTo: aMenu hand: aHandMorph
	"Construct a menu offerring embed targets for the receiver.  If the incoming menu is is not degenerate, add the constructed menu as a submenu; in any case, answer the embed-target menu"

	| menu potentialEmbeddingTargets |
	potentialEmbeddingTargets := self potentialEmbeddingTargets.
	potentialEmbeddingTargets size > 1
		ifFalse: [ ^ self ].
	menu := UIManager default newMenuIn: self for: self.
	potentialEmbeddingTargets
		reverseDo: [ :m | 
			menu
				add: m class name asString
				target: m
				selector: #addMorphFrontFromWorldPosition:
				argument: self topRendererOrSelf.
			menu lastItem icon: (m iconOrThumbnailOfSize: 16).
			self owner == m
				ifTrue: [ menu lastItem emphasis: 1 ] ].
	aMenu add: 'embed into' subMenu: menu.
	^ menu