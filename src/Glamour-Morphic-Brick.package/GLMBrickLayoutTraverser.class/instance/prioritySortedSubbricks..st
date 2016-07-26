private
prioritySortedSubbricks: aBrick
	|sorted|
	sorted := OrderedCollection newFrom: aBrick subbricks.
	sorted sort: [ :first :second | (self isDependsOnParent: first) ifFalse: [ true ]
		 ifTrue: [ (self isDependsOnParent: second) ifFalse: [ false ]
			ifTrue: [ (self hasDimensionRestrictions: first) ifFalse: [ false ]
				ifTrue: [ (self hasDimensionRestrictions: second) ifFalse: [ true ]
					ifTrue: [ (self hasMinDimensionRestriction: first) ifFalse: [ false ]
						ifTrue: [ true ] ] ] ] ] ].
	^ sorted