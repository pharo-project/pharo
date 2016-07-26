queries
copiesFromSuperclass: method
	"Checks whether the receiver copied the argument,  method, from its superclasses"
	
	self allSuperclassesDo: [ :cls|
		(cls includesSelector: method selector)
			ifTrue: [ ^ (cls >> method selector) sourceCode  = method sourceCode]].
	^ false