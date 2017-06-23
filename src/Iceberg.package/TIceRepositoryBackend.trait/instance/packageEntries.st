private
packageEntries
	self codeDirectory exists ifFalse: [ ^ #() ].
	^ self codeDirectory entries 
		select: [ :entry | self isPackageEntry: entry ]
	