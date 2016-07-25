mouse managing
clicked: event inTheCheckboxOf: aTargetMorph
	| position |

	aTargetMorph ifNil: [ ^ false ].
	position := aTargetMorph point: event position from: client.
	^ self isCheckList and: [ (aTargetMorph checkClickableZone translateBy: 2 @ 0) containsPoint: position ]