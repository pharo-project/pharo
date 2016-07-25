accessing
absolutePath
	"Return the absolute of the receiver"
	^ self path isRelative
		ifFalse: [ self path ]
		ifTrue: [ filesystem resolve: self path ]