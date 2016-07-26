macro transformations
noteSpecialSelector: selectorSymbol
	"special > 0 denotes specially treated (potentially inlined) messages. "

	special := MacroSelectors indexOf: selectorSymbol.
