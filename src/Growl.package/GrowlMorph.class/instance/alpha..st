accessing
alpha: newAlpha

	"self alpha = newAlpha ifTrue: [^ self]."
	alpha := newAlpha.
	labelAttr color: (self labelColor alpha: alpha).
	contentsAttr color: (self contentsColor alpha: alpha).
	self backgroundColor: (self nextColorStep: self backgroundColor).
	self allMorphsDo: [:m |
		m borderColor: (self nextColorStep: m borderColor).
		m color: (self nextColorStep: m color)].
	self borderColor isTransparent ifTrue: [self delete].