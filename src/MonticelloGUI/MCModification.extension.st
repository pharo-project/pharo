Extension { #name : 'MCModification' }

{ #category : '*MonticelloGUI-Diff' }
MCModification >> diff [
	"Open a diff browser on the changes."

	(DiffMorph
		from: self fromSource
		to: self toSource
		contextClass: (self isClassPatch ifTrue: [nil] ifFalse: [self targetClass]))
			extent: 400@300;
			openInWindowLabeled: 'Diff' translated
]

{ #category : '*MonticelloGUI-Diff' }
MCModification >> diffFromSource [
	"Answer fromSource of the modification. If a class patch then answer
	the fromSource with the class-side definition and comment appended."

	^self isClassPatch
		ifTrue: [self fromSource, String cr, String cr,
				obsoletion classDefinitionString, String cr, String cr,
				obsoletion commentStamp, String cr,
				obsoletion comment]
		ifFalse: [obsoletion diffSource]
]

{ #category : '*MonticelloGUI-Diff' }
MCModification >> diffToSource [
	"Answer toSource of the modification. If a class patch then answer
	the toSource with the class-side definition and comment appended."

	^self isClassPatch
		ifTrue: [self toSource, String cr, String cr,
				modification classDefinitionString, String cr, String cr,
				modification commentStamp, String cr,
				modification comment]
		ifFalse: [self toSource]
]
