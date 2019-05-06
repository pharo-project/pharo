Extension { #name : #InputTextDropList }

{ #category : #'*Spec-Examples' }
InputTextDropList class >> example [
	<sampleInstance>
	^ self new
		placeholder: 'a Number';
		displayBlock: [ :item | item asString ];
		items: {'Potatoes' . 'Carrots' . 'Onions'};
		extent: 350 @ 50;
		title: 'InputTextDropList example';
		openWithSpec;
		yourself
]
