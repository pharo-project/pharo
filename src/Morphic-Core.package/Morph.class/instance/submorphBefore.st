submorphs-accessing
submorphBefore
	"Return the submorph after (behind) me, or nil"
	| ii |
	owner ifNil: [^ nil].
	^ (ii := owner submorphIndexOf: self) = 1 
		ifTrue: [nil]
		ifFalse: [owner submorphs at: ii-1].
	
