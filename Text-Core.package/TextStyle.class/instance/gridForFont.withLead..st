private
gridForFont: fontIndex withLead: leadInteger 
	"Force whole style to suit one of its fonts. Assumes only one font referred
	to by runs."
	| font |
	font := self fontAt: fontIndex.
	self lineGrid: font height + leadInteger.
	self baseline: font ascent.
	self leading: leadInteger