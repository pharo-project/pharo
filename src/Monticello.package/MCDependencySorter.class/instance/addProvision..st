private
addProvision: anObject
	| newlySatisfied |
	provided add: anObject.
	newlySatisfied := required removeKey: anObject ifAbsent: [#()].
	self addAll: newlySatisfied.