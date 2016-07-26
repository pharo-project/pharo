private
findExactPathMatchIn: anArray 
	self
		withSiblingsDo: [:each | (each complexContents asString = anArray first
					or: [anArray first isNil])
				ifTrue: [^ each]].
	^ nil