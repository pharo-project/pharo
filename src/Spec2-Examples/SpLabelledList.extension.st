Extension { #name : #SpLabelledList }

{ #category : #'*Spec2-Examples' }
SpLabelledList class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec2-Examples' }
SpLabelledList class >> setUpExample: example [
	example items: {'item 1' . 'item 2'}
]

{ #category : #'*Spec2-Examples' }
SpLabelledList class >> simpleExample [
	<example>
	^ (self new
		layout: self labelTop ;
		label: 'list label';
		items:#('stef' 'marcus' 'sven' 'norbert' 'guille' 'esteban');
		display: [ :item | item capitalized  ];
		sortingBlock: [ :n1 :n2 | n1 last < n2 last ];
		whenSelectedItemChanged:  
			[ :item | Transcript nextPutAll: item asString;cr;endEntry  ];
		whenSelectionChanged: 
			[ :selection | Transcript nextPutAll: selection isEmpty asString;cr;endEntry ];
		whenSelectionIndexChanged: 
			[ :index | Transcript nextPutAll: index asString;cr;endEntry ])
	openWithSpec
]
