Extension { #name : #InputTextDropList }

{ #category : #'*Spec-Examples' }
InputTextDropList class >> example [
	<sampleInstance>
	| example |
	example := self new
		placeholder: 'a Number';
		displayBlock: [ :item | item asString ];
		items: {'Potatoes' . 'Carrots' . 'Onions'};
		extent: 350 @ 50;
		openWithSpec;
		yourself.
	example withWindowDo: [ :window | window title: 'InputTextDropList example' ].
	^ example
]
