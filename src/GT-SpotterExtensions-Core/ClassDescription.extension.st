Extension { #name : #ClassDescription }

{ #category : #'*GT-SpotterExtensions-Core' }
ClassDescription class >> gtExampleRPackage [
	<gtExample>
	^ GTSpotter package
]

{ #category : #'*GT-SpotterExtensions-Core' }
ClassDescription >> spotterPackageFor: aStep [
	<spotterOrder: 50>
	aStep listProcessor
			title: 'Package';
			allCandidates: [ {self package} ];
			itemName: [ :item | item packageName ];
			itemIcon: #systemIcon;
			filter: GTFilterSubstring
]
