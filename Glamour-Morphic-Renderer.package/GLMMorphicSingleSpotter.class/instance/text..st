accessing-text
text: aString
	text = aString
		ifTrue: [ ^ self ].
	text := aString.
	self listValues: (self model valuesFor: text).
	self isAssisted ifFalse: [
		self listIndex: (self listLabels size = 1
			ifTrue: [ 1 ]
			ifFalse: [ self listLabels findFirst: [ :each | each sameAs: text ] ]) ].
	self changed: #text; changed: #isEnabled