transforming
inlineSelector
	sourceMessage isNil ifTrue: [self findSelectedMessage].
	^sourceMessage selector