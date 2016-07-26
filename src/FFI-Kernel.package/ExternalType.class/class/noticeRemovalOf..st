housekeeping
noticeRemovalOf: aClass
	"A subclass of ExternalStructure is being removed.
	Clean out any obsolete references to its type."
	| type |
	type := StructTypes at: aClass name ifAbsent:[nil].
	type == nil ifFalse:[
		type newReferentClass: nil.
		type asPointerType newReferentClass: nil].
