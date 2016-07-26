submorphs-accessing
submorphAfter
	"Return the submorph after (behind) me, or nil"
	| ii |
	owner ifNil: [^ nil].
	^ (ii := owner submorphIndexOf: self) = owner submorphs size 
		ifTrue: [nil]
		ifFalse: [owner submorphs at: ii+1].
	
