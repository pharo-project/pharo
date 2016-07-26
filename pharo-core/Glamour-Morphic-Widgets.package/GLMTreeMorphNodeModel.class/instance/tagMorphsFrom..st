callbacks
tagMorphsFrom: aTagProvider
	| tagMorphs tags tagsFilter |
	tagMorphs := OrderedCollection new.
	tags:= self containerTree glamourPresentation tagsFor: self item fromProvider: aTagProvider to: #show.
	tagsFilter := self containerTree glamourPresentation tagsFor: self item fromProvider: aTagProvider to: #filter.
	tags withIndexDo:[ :each :index | 
			tagMorphs addLast: (self buttonForTag: each 
									  filter: (tagsFilter at: index ifAbsentPut:[ each ])) ].
	^ tagMorphs