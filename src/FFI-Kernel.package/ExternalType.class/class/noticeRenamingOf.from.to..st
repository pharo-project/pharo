housekeeping
noticeRenamingOf: aClass from: oldName to: newName
	"An ExternalStructure has been renamed from oldName to newName.
	Keep our type names in sync."
	| type |
	type := StructTypes at: oldName ifAbsent:[nil].
	type == nil ifFalse:[StructTypes at: newName put: type].
	StructTypes removeKey: oldName ifAbsent:[].