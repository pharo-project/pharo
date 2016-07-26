accessing
description
	"For big value to print, overwrite description block"
	descriptionBlock ifNil: [ ^ self value asString ].
	^ descriptionBlock cull: self value