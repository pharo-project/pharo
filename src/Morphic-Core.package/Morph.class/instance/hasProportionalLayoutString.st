layout-menu
hasProportionalLayoutString
	| layout |
	^ ((layout := self layoutPolicy) notNil
			and: [layout isProportionalLayout]) -> 'proportional layout' translated