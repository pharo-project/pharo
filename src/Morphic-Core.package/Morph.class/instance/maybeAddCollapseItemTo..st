menus
maybeAddCollapseItemTo: aMenu
	"If appropriate, add a collapse item to the given menu"

	| anOwner |
	(anOwner := self topRendererOrSelf owner) ifNotNil:
			[anOwner isWorldMorph ifTrue:
				[aMenu add: 'collapse' target: self selector: #collapse]]