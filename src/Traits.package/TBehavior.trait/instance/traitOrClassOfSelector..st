traits
traitOrClassOfSelector: aSymbol
	"Return the trait or the class which originally defines the method aSymbol
	or return self if locally defined or if it is a conflict marker method.
	This is primarly used by Debugger to determin the behavior in which a recompiled
	method should be put. If a conflict method is recompiled it should be put into
	the class, thus return self. Also see TraitComposition>>traitProvidingSelector:"
	
	((self includesLocalSelector: aSymbol) or: [
		self hasTraitComposition not]) ifTrue: [^self].
	^(self traitComposition traitProvidingSelector: aSymbol) ifNil: [self]