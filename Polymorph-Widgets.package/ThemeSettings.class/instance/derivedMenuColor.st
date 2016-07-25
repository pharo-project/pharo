menu
derivedMenuColor
	"Answer a selection colour based on the window color."

	| autoColor worldColor |
	autoColor := self autoMenuColor 
				and: [Display depth > 4
						and: [(worldColor := self currentWorld color) isColor]].

	^ autoColor
				ifTrue: [worldColor luminance > 0.7
						ifTrue: [worldColor mixed: 0.85 with: Color black]
						ifFalse: [worldColor mixed: 0.4 with: Color white]]
				ifFalse: [self menuColor].
