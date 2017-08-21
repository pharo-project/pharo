queries
copiesMethodsFromSuperclass
	"Checks whether the receiver copied some method from its superclass"

	self methodsDo: [ :method|
		(self copiesFromSuperclass: method)
			ifTrue: [ ^ true ]].
	^ false