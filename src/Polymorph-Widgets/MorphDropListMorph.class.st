"
Drop list supporting morphs in list.
"
Class {
	#name : #MorphDropListMorph,
	#superclass : #DropListMorph,
	#category : #'Polymorph-Widgets-Base'
}

{ #category : #accessing }
MorphDropListMorph >> font [
	"Answer the content font"

	^self listFont
]

{ #category : #accessing }
MorphDropListMorph >> font: aFont [
	"Set the list font"

	self listFont: aFont
]

{ #category : #private }
MorphDropListMorph >> listMorphClass [
	"Answer the class for a new list morph"

	^PluggableMorphListMorph
]

{ #category : #drawing }
MorphDropListMorph >> newContentMorph [
	"Answer a new content morph"

	^Morph new
		changeTableLayout;
		listDirection: #leftToRight;
		wrapCentering: #center;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		layoutInset: 2;
		color: Color transparent;
		borderWidth: 0;
		clipSubmorphs: true;
		lock
]

{ #category : #updating }
MorphDropListMorph >> updateContentColor: paneColor [
	"Change the content text color."
	
]

{ #category : #testing }
MorphDropListMorph >> updateContents [
	"Update the contents."

	|item|
	self contentMorph removeAllMorphs.
	self listSelectionIndex > 0
		ifTrue: [item := (self list at: self listSelectionIndex) copy
					hResizing: #spaceFill;
					vResizing: #rigid.
				self contentMorph
					addMorph: item]
]
