instance variables
instVarNameForIndex: index
	"Answer the named instance variable with index index or nil if none."

	| superInstSize |
	index > self instSize ifTrue: [^nil].
	superInstSize := self superclass ifNil: [0] ifNotNil: [self superclass instSize].
	index > superInstSize ifTrue:
		[^self instVarNames at: index - superInstSize].
	self superclass ifNil: [^nil].
	^self superclass instVarNameForIndex: index

	"(Object allSubclasses select:
		[:cls| cls instSize > cls superclass instSize and: [cls subclasses isEmpty and: [cls superclass instSize > 0]]]) collect:
			[:cls| (1 to: cls instSize) collect: [:i| cls instVarNameForIndex: i]]"