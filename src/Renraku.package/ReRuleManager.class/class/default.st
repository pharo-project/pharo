instance creation
default
	^ default ifNil: [ default := self newWithRules: self defaultRules ]