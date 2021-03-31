Extension { #name : #SpLabelledDropList }

{ #category : #'*Spec2-Examples' }
SpLabelledDropList class >> example [
	<sampleInstance>
	^ super example
]

{ #category : #'*Spec2-Examples' }
SpLabelledDropList class >> setUpExample: example [
	example
		items: {'item 1' . 'item 2'};
		display: [ :item | item asString ];
		extent: 400 @ 50
]

{ #category : #'*Spec2-Examples' }
SpLabelledDropList class >> simpleExample [
	<example>
	^ (self new
		label: 'droplist label';
		items:#('stef' 'marcus' 'sven' 'norbert' 'guille' 'esteban');
		display: [ :item | item capitalized  ];
		setIndex: 5;
		whenSelectedItemChanged:  
			[ :item | Transcript nextPutAll: item asString;cr;endEntry  ];
		whenSelectionChanged: 
			[ :selection | Transcript nextPutAll: selection isEmpty asString;cr;endEntry ];
		whenSelectionIndexChanged: 
			[ :index | Transcript nextPutAll: index asString;cr;endEntry ]	)
	openWithSpec
]
