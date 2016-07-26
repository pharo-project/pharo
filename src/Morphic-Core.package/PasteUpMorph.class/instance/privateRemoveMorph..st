private
privateRemoveMorph: aMorph
	backgroundMorph == aMorph ifTrue: [ backgroundMorph := nil ].
	^super privateRemoveMorph: aMorph.
