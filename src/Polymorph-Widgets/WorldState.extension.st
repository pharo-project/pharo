Extension { #name : 'WorldState' }

{ #category : '*Polymorph-Widgets' }
WorldState class >> quitSession [
	| response |
	response := MorphicUIManager new
						confirm: 'Save changes before quitting?'
						trueChoice: 'Save'
						falseChoice: 'Discard'
						cancelChoice: 'Cancel'
						default: nil.
	response ifNil: [ ^self ].
	response
		ifTrue: [Smalltalk snapshot: true andQuit: true]
		ifFalse: [Smalltalk snapshot: false andQuit: true]
]

{ #category : '*Polymorph-Widgets' }
WorldState class >> saveAs [

	| reference |
	reference := MorphicUIManager new
		             chooseForSaveFileReference: 'Save Image as?'
		             extensions: #( 'image' )
		             path: Smalltalk imageFile nextVersion.

	reference ifNotNil: [
		Smalltalk saveAs:
			reference parent / (reference basenameWithoutExtension: 'image') ]
]
