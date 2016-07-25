layout-menu
hasTableLayoutString
	| layout |
	^ ((layout := self layoutPolicy) notNil
			and: [layout isTableLayout]) ->   'table layout' translated