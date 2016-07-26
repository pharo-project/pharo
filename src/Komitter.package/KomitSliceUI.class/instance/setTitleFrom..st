private
setTitleFrom: aZnResponse

	(aZnResponse isError or: [ aZnResponse entity isNil ])
		ifTrue: [ 
			titleWidget text: 'Error !'.
			loadingWidget error ]
		ifFalse: [ 
			titleWidget text: (self encodeTitle: aZnResponse entity string).
			self valid: true.
			loadingWidget ok ]