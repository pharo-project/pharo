compiling
compileAllFrom: oldClass
	"Recompile all the methods in the receiver's method dictionary (not the
	subclasses). Also recompile the methods in the metaclass."

	super compileAllFrom: oldClass.
	self theMetaClass compileAllFrom: oldClass theMetaClass