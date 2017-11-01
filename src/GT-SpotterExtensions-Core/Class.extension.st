Extension { #name : #Class }

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> definitionForSpotter [
	| str |
	str := WriteStream on: String new.
	str nextPutAll: self definitionForNautilus.
	str cr; cr.
	str nextPut: $".
	str nextPutAll: 'Hierarchy: '.
	str nextPutAll: ($> join: self allSuperclasses reversed).
	str cr; cr.
	self hasComment ifTrue: [ 
		str nextPutAll: self comment.
	].	
	str nextPut: $".
	^ str contents
]

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> spotterActDefault [
	^ self browse
]

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> spotterClassMethodsFor: aStep [
	<spotterOrder: 15>
	aStep listProcessor
			title: 'Class methods';
			allCandidates: [ self classSide methods ];
			itemName: [ :method | method selector ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> spotterMethodsFor: aStep [
	<spotterOrder: 10>
	aStep listProcessor
			title: 'Instance methods';
			allCandidates: [ self methods ];
			itemName: [ :method | method selector ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	aComposite pharoMethod
		title: [self name];
		display: #definitionForNautilus;
		smalltalkClass: [ nil ];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> spotterReferenceFor: aStep [
	<spotterOrder: 30>
	aStep listProcessor
			title: 'References';
			allCandidates: [ (SystemNavigation default allReferencesTo: self binding) collect: #compiledMethod ];
			itemName: [ :method | method gtDisplayString ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> spotterSuperClassMethodsFor: aStep [
	<spotterOrder: 16>
	|superclasses|
	superclasses := self class withAllSuperclasses select: #isMeta.
	aStep listProcessor
			title: 'Super class methods';
			allCandidates: [ self classSide allMethods select: [ :each |(each methodClass = self classSide) not & (superclasses includes: each methodClass) ] ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
Class >> spotterSuperMethodsFor: aStep [
	<spotterOrder: 11>
	aStep listProcessor
			title: 'Super instance methods';
			allCandidates: [ self allMethods reject: [ :each | each methodClass = self ] ];
			filter: GTFilterSubstring
]
