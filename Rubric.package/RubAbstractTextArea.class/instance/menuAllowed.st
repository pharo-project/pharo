accessing
menuAllowed
	^ menuAllowed ifNil: [ menuAllowed := self defaultMenuAllowed  ]