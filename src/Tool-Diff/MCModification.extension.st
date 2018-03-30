Extension { #name : #MCModification }

{ #category : #'*Tool-Diff' }
MCModification >> diff [
	"Open a diff browser on the changes."

	(DiffMorph
		from: self fromSource
		to: self toSource
		contextClass: (self isClassPatch ifTrue: [nil] ifFalse: [self targetClass]))
			extent: 400@300;
			openInWindowLabeled: 'Diff' translated
]

{ #category : #'*Tool-Diff' }
MCModification >> diffFromSource [
	"Answer fromSource of the modification. If a class patch then answer
	the fromSource with the class-side definition and comment appended."

	^ self isClassPatch
		ifTrue: [ self fromSource , String newLine , String newLine , obsoletion classDefinitionString , String newLine , String newLine
				, obsoletion commentStamp , String newLine , obsoletion comment ]
		ifFalse: [ obsoletion diffSource ]
]

{ #category : #'*Tool-Diff' }
MCModification >> diffToSource [
	"Answer toSource of the modification. If a class patch then answer
	the toSource with the class-side definition and comment appended."

	^ self isClassPatch
		ifTrue: [ self toSource , String newLine , String newLine , modification classDefinitionString , String newLine , String newLine
				, modification commentStamp , String newLine , modification comment ]
		ifFalse: [ self toSource ]
]
