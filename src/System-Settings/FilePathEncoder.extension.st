Extension { #name : #FilePathEncoder }

{ #category : #'*System-Settings' }
FilePathEncoder class >> settingInputWidgetForNode: aSettingNode [
	^ aSettingNode  inputWidgetForFileOrDirectoryWithAction: #chooseFilePath.
]
