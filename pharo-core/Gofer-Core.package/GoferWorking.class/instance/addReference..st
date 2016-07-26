private
addReference: aReference
	| workingCopy |
	workingCopy := aReference workingCopyIfAbsent: [ ^ self ].
	(self workingCopies includes: workingCopy)
		ifTrue: [ ^ self ].
	self workingCopies addLast: workingCopy.
	workingCopy requiredPackages do: [ :package |
		self addReference: (GoferPackageReference
			name: package name) ]