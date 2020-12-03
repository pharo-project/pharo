Extension { #name : #UITheme }

{ #category : #'*NewTools-Inspector-Extensions' }
UITheme >> inspectionColors [
	<inspectorPresentationOrder: 10 title: 'Colors'>
	
	^ SpListPresenter new 
		items: ((UITheme selectors select: [ :each | 
			(each endsWith: 'Color') and: [ ((self perform: each) isKindOf: Color) ] ])
			sort: #yourself ascending);
		display: [ :each | each ];
		displayIcon: [ :each | 
			Morph new 
				color: (self perform: each);
				asFormOfSize: 20@20 ];
		yourself
]
