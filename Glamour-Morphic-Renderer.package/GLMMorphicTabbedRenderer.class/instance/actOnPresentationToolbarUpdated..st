rendering
actOnPresentationToolbarUpdated: ann
	| presentations |

	presentations := ann presentation matchingPresentations.
	tabs pages size to: 1 by: -1 do: [ :i | (tabs pages at: i) toolbar: GLMMorphic emptyMorph ].	
	presentations withIndexDo: [ :each :i | 
		tabs updatePageAtIndex: i
			withLabel: (self titleOrIconOf: each in: tabs)
			toolbar: (self renderToolbarForPresentation: each inMorph: tabs)
			action: (self labelActionMorphFor: each)]