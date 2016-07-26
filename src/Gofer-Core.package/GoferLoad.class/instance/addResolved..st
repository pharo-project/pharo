private
addResolved: aResolvedReference
	| version |
	version := aResolvedReference version.
	(self versions includes: version)
		ifTrue: [ ^ self ].
	self versions addLast: version.
	version dependencies do: [ :dependency |
		self addResolved: ((GoferVersionReference
			name: dependency versionInfo name) resolveWith: self gofer) ]