structure
topRendererOrSelf
	"Answer the topmost renderer for this morph, or this morph itself if it has no renderer. See the comment in Morph>isRenderer."

	| top topsOwner |
	owner ifNil: [^self].
	self isWorldMorph ifTrue: [^self].	"ignore scaling of this world"
	top := self.
	topsOwner := top owner.
	[topsOwner notNil and: [topsOwner isRenderer]] whileTrue: 
			[top := topsOwner.
			topsOwner := top owner].
	^top