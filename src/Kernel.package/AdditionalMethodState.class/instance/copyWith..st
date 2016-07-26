copying
copyWith: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	(Association == aPropertyOrPragma class
	 or: [Pragma == aPropertyOrPragma class]) ifFalse:
		[self error: self class name, ' instances should hold only Associations or Pragmas.'].
	"no need to initialize here; we're copying all inst vars"
	copy := self class basicNew: (bs := self basicSize) + 1.
	1 to: bs do:
		[:i|
		copy basicAt: i put: (self basicAt: i) shallowCopy].
	copy basicAt: bs + 1 put: aPropertyOrPragma.
	1 to: self class instSize do:
		[:i| copy instVarAt: i put: (self instVarAt: i)].
	^copy