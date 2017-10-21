private
linkSplittersToSplitters
	"The pane morphs are already linked. Cross link the splitters as appropriate."
	
	self splitters do: [:each |
		each splitsTopAndBottom
			ifTrue: [self splitters do: [:eachMorph |
					eachMorph splitsTopAndBottom ~= each splitsTopAndBottom ifTrue: [
						eachMorph layoutFrame bottomFraction = each layoutFrame topFraction
							ifTrue: [each addLeftOrTop: eachMorph].
						eachMorph layoutFrame topFraction = each layoutFrame bottomFraction
							ifTrue: [each addRightOrBottom: eachMorph]]]]
			ifFalse: [self splitters do: [:eachMorph |
					eachMorph splitsTopAndBottom ~= each splitsTopAndBottom ifTrue: [
						eachMorph layoutFrame rightFraction = each layoutFrame leftFraction
							ifTrue: [each addLeftOrTop: eachMorph].
						eachMorph layoutFrame leftFraction = each layoutFrame rightFraction
							ifTrue: [each addRightOrBottom: eachMorph]]]].
				
		each comeToFront.]