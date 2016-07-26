event handling
openFloatingEditorToRenameFromNodeMorph: aNodeMorph
	| ed pkgMorph font element |
	element := aNodeMorph complexContents item.
	pkgMorph := aNodeMorph lastSubmorph.
	pkgMorph hasSubmorphs
		ifTrue: [ pkgMorph := pkgMorph lastSubmorph ].
	font := pkgMorph font.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith:
				[ :editor | 
			| bnds |
			editor font: font.
			editor scrollbarsShowNever.
			bnds := pkgMorph topLeft - (2@0) extent: (aNodeMorph owner width - pkgMorph left) @ (aNodeMorph height).
			editor bounds: (aNodeMorph owner transform localBoundsToGlobal: bnds) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newName |
			newName := editedContents asString trimBoth.
			(newName = element name asString or: [ newName isEmpty])
				ifFalse:
					[ [ element renameTo: newName ]
						on: RPackageConflictError
						do: [ ^ self inform: newName , ' already exists' ].
					self model model selectedPackage: element.
					self model model updatePackageView ] ].
	ed openEditorWithContents: element name asString thenDo: [ :editor | editor selectAll  ]