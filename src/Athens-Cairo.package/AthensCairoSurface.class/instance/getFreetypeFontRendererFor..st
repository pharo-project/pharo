text support
getFreetypeFontRendererFor: aFreetypeFont 
	"answer the same instance, just reset it's font and advance"
	
	^ ftFontRenderer font: aFreetypeFont; advance: 0@0; yourself