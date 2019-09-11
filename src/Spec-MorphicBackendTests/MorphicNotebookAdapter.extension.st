Extension { #name : #MorphicNotebookAdapter }

{ #category : #'*Spec-MorphicBackendTests' }
MorphicNotebookAdapter >> clickTab: anIndex [
	| evt tab |
	evt := MouseButtonEvent new
		setType: nil
		position: widget center
		which: MouseButtonEvent redButton
		buttons: MouseButtonEvent redButton
		hand: nil
		stamp: nil.
	tab := self widget tabSelectorMorph tabs at: anIndex.

	(tab handlesMouseDown: evt)
		ifTrue: [ tab mouseDown: evt.
			tab mouseUp: evt ]
]

{ #category : #'*Spec-MorphicBackendTests' }
MorphicNotebookAdapter >> numberOfTabs [
	^ self widget tabSelectorMorph tabs size
]

{ #category : #'*Spec-MorphicBackendTests' }
MorphicNotebookAdapter >> selectedPageName [
	^ (self widget tabSelectorMorph selectedTab label submorphs detect: [ :each | each isKindOf: StringMorph ]) contents
]
