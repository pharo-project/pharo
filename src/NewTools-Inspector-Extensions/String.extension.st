Extension { #name : #String }

{ #category : #'*NewTools-Inspector-Extensions' }
String >> inspectionString [
	<inspectorPresentationOrder: -10 title: 'String'>
	
	^ SpTextPresenter new
		text: (self truncateWithElipsisTo: 100000);
		yourself
]
