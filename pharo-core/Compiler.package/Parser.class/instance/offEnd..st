error handling
offEnd: aString 
	"Notify a problem beyond 'here' (in lookAhead token). Don't be offEnded!"

	requestorOffset == nil
		ifTrue: [^ self notify: aString at: mark]
		ifFalse: [^ self notify: aString at: mark + requestorOffset]
