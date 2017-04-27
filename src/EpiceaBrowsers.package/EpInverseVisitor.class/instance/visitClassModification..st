visitor
visitClassModification: aChange

	^ EpClassModification oldClass: aChange newClass newClass: aChange oldClass.
