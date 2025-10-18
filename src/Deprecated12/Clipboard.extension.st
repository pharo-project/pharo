Extension { #name : 'Clipboard' }

{ #category : '*Deprecated12' }
Clipboard class >> chooseRecentClipping [
	"Deprecated without replacement. Check #recentClippings"

	self deprecated: 'This method will be removed in the next version of Pharo.'.
	^ self default chooseRecentClipping
]

{ #category : '*Deprecated12' }
Clipboard >> chooseRecentClipping [
	"Deprecated without replacement. Check #recentClippings"

	self deprecated: 'This method will be removed in the next version of Pharo.'.

	recentClippings ifNil: [ ^ nil ].
	^ MorphicUIManager new
		  chooseFrom: (recentClippings collect: [ :txt |
				   ((txt asString contractTo: 50)
					    copyReplaceAll: String cr
					    with: '\') copyReplaceAll: String tab with: '|' ])
		  values: recentClippings
]
