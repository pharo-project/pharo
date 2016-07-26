page
relabelPage: aMorph with: aStringOrMorph
	"Relabel the tab associated with the given page and
	update tabs."

	self tabSelectorMorph
		relabelTab: (self tabOfPage: aMorph) with: aStringOrMorph