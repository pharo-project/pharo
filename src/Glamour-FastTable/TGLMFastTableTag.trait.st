"
I am a Trait class use to manage some basic functionnalities needed to manage tags in a Gl̂amour data source.

My user need to define the #tagToFilterBy which should return a collection of tag, then I manage some basics needs.
"
Trait {
	#name : #TGLMFastTableTag,
	#category : #Glamour-FastTable-Model
}

{ #category : #factory }
TGLMFastTableTag >> buttonForTag: each filter: filter [
	^ (GLMMorphic togglingButtonLabelled: each pressed: (self shouldFilterByTag: filter) style: self glamourPresentation tagsStyle)
		target: self;
		actionSelector: #toggleFilteringByTag:;
		arguments: (Array with: filter);
		yourself
]

{ #category : #private }
TGLMFastTableTag >> resetChildrenContent [
	self explicitRequirement
]

{ #category : #accessing }
TGLMFastTableTag >> shouldFilterByTag: aTag [
	^ self tagsToFilterBy includes: aTag 
]

{ #category : #factory }
TGLMFastTableTag >> tagMorphsFrom: aTagProvider for: element [
	| tagMorphs tags tagsFilter |
	
	tagMorphs := OrderedCollection new.
	tags:= self glamourPresentation tagsFor: element fromProvider: aTagProvider to: #show.
	tagsFilter := self glamourPresentation tagsFor: element fromProvider: aTagProvider to: #filter.
	tags withIndexDo:[ :each :index | 
		tagMorphs addLast: (self 
			buttonForTag: each 
			filter: (tagsFilter at: index ifAbsentPut:[ each ])) ].
	^ tagMorphs
]

{ #category : #accessing }
TGLMFastTableTag >> tagsToFilterBy [
	self explicitRequirement
]

{ #category : #callbacks }
TGLMFastTableTag >> toggleFilteringByTag: aTag [
	(self shouldFilterByTag: aTag)
		ifTrue: [ self tagsToFilterBy remove: aTag ]
		ifFalse: [ self tagsToFilterBy add: aTag ].
	self resetChildrenContent.
	self tableRefresh
]
