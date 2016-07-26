queries
copiedFromSuperclass: method
	"Returns the methods that the receiver copied with its ancestors"
	
	self allSuperclassesDo: [ :cls|
		(cls includesSelector: method selector)
			ifTrue: [ 
				((cls >> method selector) sourceCode  = method sourceCode)
					ifTrue: [ ^ {cls >> method selector}]
					ifFalse: [ ^ #()]]].
	^ #().
	