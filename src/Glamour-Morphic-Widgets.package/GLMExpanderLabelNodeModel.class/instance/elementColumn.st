callbacks
elementColumn
	| row tags tagsFilter |
	row := OrderedCollection with: (self displayText).
	
"	tags:= self containerTree glamourPresentation tagsFor: self item to: #show.
	tagsFilter := self containerTree glamourPresentation tagsFor: self item to: #filter.
	tags withIndexDo:[ :each :index | 
			row addLast: (self buttonForTag: each 
									  filter: (tagsFilter at: index ifAbsentPut:[ each ])) ].
"	^ (UITheme current newRowIn: World for: row) fillStyle: Color veryLightGray