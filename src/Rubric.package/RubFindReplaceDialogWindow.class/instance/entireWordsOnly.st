accessing
entireWordsOnly
	^ self model ifNil: [false] ifNotNil: [:m | m entireWordsOnly] 