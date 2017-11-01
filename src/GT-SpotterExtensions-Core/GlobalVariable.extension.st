Extension { #name : #GlobalVariable }

{ #category : #'*GT-SpotterExtensions-Core' }
GlobalVariable >> spotterActDefault [
	self value inspect
]

{ #category : #'*GT-SpotterExtensions-Core' }
GlobalVariable >> spotterClassFor: aStep [
	<spotterOrder: 50>
	aStep listProcessor
			title: 'Class';
			allCandidates: [ { self value class }];
			itemIcon: #systemIcon;
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
GlobalVariable >> spotterForReferencesFor: aStep [
	<spotterOrder: 130>
	aStep listProcessor
			title: 'References';
			allCandidates: [ (SystemNavigation default allReferencesTo: self) collect: #compiledMethod ];
			itemName: [ :method | method gtDisplayString ];
			filter: GTFilterSubstring
]
