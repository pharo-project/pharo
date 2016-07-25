private
headers
	headers isNil ifTrue: [ headers := ZnMultiValueDictionary new ].
	^ headers