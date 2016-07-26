factory
tagMorphsFrom: aTagProvider for: element
	| tagMorphs tags tagsFilter |
	
	tagMorphs := OrderedCollection new.
	tags:= self glamourPresentation tagsFor: element fromProvider: aTagProvider to: #show.
	tagsFilter := self glamourPresentation tagsFor: element fromProvider: aTagProvider to: #filter.
	tags withIndexDo:[ :each :index | 
		tagMorphs addLast: (self 
			buttonForTag: each 
			filter: (tagsFilter at: index ifAbsentPut:[ each ])) ].
	^ tagMorphs