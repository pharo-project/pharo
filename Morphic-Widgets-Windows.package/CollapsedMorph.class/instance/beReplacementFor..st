collapse/expand
beReplacementFor: aMorph

	| itsWorld priorPosition |
	(itsWorld := aMorph world) ifNil: [^self].
	uncollapsedMorph := aMorph.
			
	self setLabel: aMorph externalName.
	aMorph delete.
	itsWorld addMorphFront: self.
	self collapseOrExpand.
	(priorPosition := aMorph valueOfProperty: #collapsedPosition ifAbsent: [nil])
	ifNotNil:
		[self position: priorPosition].
