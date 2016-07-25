accessing-classes
metaclassNamed: aSymbol 
	| class |
	aSymbol isNil ifTrue: [^nil].
	(self hasRemoved: aSymbol) ifTrue: [^nil].
	(newClasses includesKey: aSymbol) 
		ifTrue: [^(newClasses at: aSymbol) last].
	(changedClasses includesKey: aSymbol) 
		ifTrue: [^(changedClasses at: aSymbol) last].

	class := environment at: aSymbol ifAbsent: [nil].
	(class isBehavior or: [class isTrait])
		ifTrue: [^ (self createNewClassFor: class) last].
	^ nil