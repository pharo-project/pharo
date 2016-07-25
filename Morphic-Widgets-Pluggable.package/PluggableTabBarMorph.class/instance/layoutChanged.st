actions
layoutChanged
	"Fix up our tabs bounds"
	| tabsCount |
	super layoutChanged.
	tabsCount := self tabs size.
	tabsCount isZero ifFalse: [ | tabInnerExtent count |
		tabInnerExtent := ((self width -
				((self tabs first key outerGap + self tabs last key outerGap) // 2)
					- tabsCount)
			 		// tabsCount)
			@ (self height).
		count := 1.
		self tabs do: [ :anAssociation | | tab |
			tab := anAssociation key.
			tab innerExtent: tabInnerExtent.
			count = 1
				ifTrue: [tab position: self position]
				ifFalse: [
					tab position:
						(self position translateBy:
							((tabInnerExtent x + 1) * (count - 1))@0)].
			count := count + 1  ]	].
	self changed.