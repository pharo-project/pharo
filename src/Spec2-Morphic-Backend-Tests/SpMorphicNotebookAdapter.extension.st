Extension { #name : #SpMorphicNotebookAdapter }

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMorphicNotebookAdapter >> clickTab: anIndex [
	| evt tab |

	tab := self widget tabSelectorMorph tabs at: anIndex.
	evt := MouseButtonEvent new
		setType: nil
		position: widget center
		which: MouseButtonEvent redButton
		buttons: MouseButtonEvent redButton
		hand: tab activeHand
		stamp: nil.

	(tab handlesMouseDown: evt)
		ifTrue: [ tab handleMouseDown: evt.
			tab handleMouseDown: evt ]
]

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMorphicNotebookAdapter >> numberOfTabs [
	^ self widget tabSelectorMorph tabs size
]

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMorphicNotebookAdapter >> selectedPageName [

	^ (self widget tabSelectorMorph selectedTab label submorphs
		detect: [ :each | each isKindOf: StringMorph ]) 
		contents
]
