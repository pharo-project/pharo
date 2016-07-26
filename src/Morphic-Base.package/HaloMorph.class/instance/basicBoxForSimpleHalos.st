private
basicBoxForSimpleHalos
	| w |
	w := self world ifNil:[target outermostWorldMorph].
	^ ((self worldBoundsForMorph: target topRendererOrSelf) expandBy: self handleAllowanceForIconicHalos)
			intersect: (w bounds insetBy: 8@8) ifNone: [self error: 'should not happen' ]
