accessing
listMorph
	listMorph ifNil: [
		"crate this lazily, in case the morph is legacy"
		listMorph := self listMorphClass new.
		listMorph listSource: self.
		listMorph width: self scroller width.
		listMorph color: self textColor ].

	listMorph owner ~~ self scroller ifTrue: [
		"list morph needs to be installed.  Again, it's done this way to accomodate legacy PluggableListMorphs"
		self scroller removeAllMorphs.
		self scroller addMorph: listMorph ].

	^listMorph