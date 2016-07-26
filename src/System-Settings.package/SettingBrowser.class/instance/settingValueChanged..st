updating
settingValueChanged: aSetting 
	aSetting realValue = true
		ifTrue: [self openPathFrom: aSetting].
	aSetting realValue = false
		ifTrue: [self closePathFrom: aSetting].
	aSetting changed: #realValue.
	aSetting changed: #fontButtonLabel.
	aSetting changed: #index.
	aSetting changed: #content 