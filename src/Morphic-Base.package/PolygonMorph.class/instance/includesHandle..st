private
includesHandle: aMorph

	handles ifNil: [^ false].
	^ handles includes: aMorph