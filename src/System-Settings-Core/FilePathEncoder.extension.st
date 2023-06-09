Extension { #name : #FilePathEncoder }

{ #category : #'*System-Settings-Core' }
FilePathEncoder class >> settingInputWidgetForNode: aSettingNode [
	^ aSettingNode  inputWidgetForFileOrDirectoryWithAction: #chooseFilePath
]
