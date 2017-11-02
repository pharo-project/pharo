Extension { #name : #TraitDescription }

{ #category : #'*GT-SpotterExtensions-Core' }
TraitDescription >> spotterPackageFor: aStep [
	<spotterOrder: 50>
	aStep listProcessor
			title: 'Package';
			allCandidates: [ {self package} ];
			itemName: [ :item | item packageName ];
			itemIcon: #systemIcon;
			filter: GTFilterSubstring
]
