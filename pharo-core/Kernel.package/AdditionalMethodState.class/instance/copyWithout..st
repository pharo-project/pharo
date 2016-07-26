copying
copyWithout: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which no longer includes aPropertyOrPragma"
	| bs copy offset |
	"no need to initialize here; we're copying all inst vars"
	copy := self class basicNew: (bs := self basicSize) - ((self includes: aPropertyOrPragma)
															ifTrue: [1]
															ifFalse: [0]).
	offset := 0.
	1 to: bs do:
		[:i|
		(self basicAt: i) = aPropertyOrPragma
			ifTrue: [offset := 1]
			ifFalse: [copy basicAt: i - offset put: (self basicAt: i) shallowCopy]].
	1 to: self class instSize do:
		[:i| copy instVarAt: i put: (self instVarAt: i)].
	^copy