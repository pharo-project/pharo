brick-structure
subbrickAfter

	"Return the subbbrick after (behind) me, or nil"
	| link |
	link := self brickDoubleLink.
	link ifNil: [ ^ nil ].
	link nextLink ifNil: [ ^ nil ].
	^ link nextLink value