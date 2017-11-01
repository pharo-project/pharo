Extension { #name : #Morph }

{ #category : #'*GT-SpotterExtensions-Core' }
Morph >> spotterForKeysFor: aStep [
	<spotterOrder: 15>
	(self hasProperty: #kmDispatcher) ifFalse: [^self].
	self kmDispatcher spotterForKeysFor: aStep
]

{ #category : #'*GT-SpotterExtensions-Core' }
Morph >> spotterForSubmorphsFor: aStep [
	<spotterOrder: 10>
	self submorphs ifNil: [ ^ self ].
	
	aStep listProcessor
			title: 'Submorphs';
			allCandidates: [ self submorphs ];
			candidatesLimit: 5;
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
Morph >> spotterPreviewMorphIn: aComposite [
	<spotterPreview: 20>
	aComposite custom: 
		(self imageForm asAlphaImageMorph asBrick margin: 20)
]
