Extension { #name : #SettingNode }

{ #category : #'*GT-SpotterExtensions-Core' }
SettingNode class >> gtExampleSettingNode [
	<gtExample>
	<label: 'Settings node'>
	
	^ SettingTree gtExampleSettingTree nodeList anyOne
]

{ #category : #'*GT-SpotterExtensions-Core' }
SettingNode >> spotterForSettingsFor: aStep [
	<spotterOrder: 0>
	self allChildren isEmpty ifTrue: [ ^ self ].
	aStep listProcessor
		title: 'Children';
		allCandidates: [ self allChildren ];
		itemName: [ :each | each spotterLabel ];
		filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
SettingNode >> spotterLabel [
	| label |
	label := self settingDeclaration label.
	^ self parentNode isNil
		ifTrue: [ label ]
		ifFalse: [ self parentNode spotterLabel , '/', label ]
		
]

{ #category : #'*GT-SpotterExtensions-Core' }
SettingNode >> spotterPreviewIn: composite [
	<spotterPreview: 0>
	
	composite custom: (
				(self settingDeclaration inputWidget asBrick)
						hSpaceFill;
						vShrinkWrap;
						vAlign: #center;
						yourself)
]
