changing suspended state
restartTopWith: method
	"Rollback top context and replace with new method.  Assumes self is suspended"

	method isQuick 
		ifTrue: [ self popTo: suspendedContext sender ]
		ifFalse: [ suspendedContext privRefreshWith: method ].
