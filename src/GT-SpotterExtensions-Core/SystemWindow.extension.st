Extension { #name : #SystemWindow }

{ #category : #'*GT-SpotterExtensions-Core' }
SystemWindow >> spotterActDefault [
	^ self isTopWindow
		ifTrue: [self comeToFront] "rise above non-window morphs"
		ifFalse:[	self activate].

]

{ #category : #'*GT-SpotterExtensions-Core' }
SystemWindow >> spotterWindowsFor: aStep [
	<spotterOrder: 100>

	aStep listProcessor
			title: 'Windows';
			allCandidates: [ self class allSubInstances ];
			itemName: [ :window | '**', window label, '**' ];
			filter: GTFilterSubstring
]
