accessing
contents
	"Answer the contents of the file, reading it first if needed."
	"Possible brevityState values:
		FileList,
		fullFile, briefFile, needToGetFull, needToGetBrief,
		fullHex, briefHex, needToGetFullHex, needToGetBriefHex"
	self reference isReadable 
		ifFalse: [ ^ 'cannot read {1}' translated format: { reference }].
	(listIndex = 0) | (brevityState == #FileList) ifTrue: [^ self defaultContents].  "no file selected"
	brevityState == #fullFile ifTrue: [^ contents]. 
	brevityState == #fullHex ifTrue: [^ contents].
	brevityState == #briefFile ifTrue: [^ contents].
	brevityState == #briefHex ifTrue: [^ contents].

	brevityState == #needToGetFullHex ifTrue: [^ self readContentsHex: false].
	brevityState == #needToGetBriefHex ifTrue: [^ self readContentsHex: true].

	brevityState == #needToGetFull ifTrue: [^ self readContentsBrief: false].
	brevityState == #needToGetBrief ifTrue: [^ self readContentsBrief: true].  "default"
	
	self error: 'unknown state ' , brevityState printString