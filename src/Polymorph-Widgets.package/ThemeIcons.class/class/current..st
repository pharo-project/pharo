instance creation
current: aPack
	aPack hasIcons ifFalse: [ aPack loadIconsFromUrl ].
	Current := aPack.
	"Polymorph depends on Morphic, so coupling with WorldState is ok, the problem is with 
	 other tools (like Nautilus), that may need to refresh its icon caches"
	World resetWorldMenu.
	SystemAnnouncer uniqueInstance announce: IconSetChanged