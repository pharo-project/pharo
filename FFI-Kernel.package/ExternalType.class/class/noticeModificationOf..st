housekeeping
noticeModificationOf: aClass
	"A subclass of ExternalStructure has been redefined.
	Clean out any obsolete references to its type."
	| type |
	aClass isBehavior ifFalse:[^nil]. "how could this happen?"
	aClass withAllSubclassesDo:[:cls|
		type := StructTypes at: cls name ifAbsent:[nil].
		type == nil ifFalse:[
			type newReferentClass: cls.
			type asPointerType newReferentClass: cls].
	].