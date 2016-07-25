display
chooseFrom: anArray title: aString lines: aCollection 
	anArray isEmpty
		ifTrue: [ ^ nil ].
	anArray size = 1
		ifTrue: [ ^ anArray first ].
	^ UIManager default chooseOrRequestFrom: anArray lines: aCollection title: aString