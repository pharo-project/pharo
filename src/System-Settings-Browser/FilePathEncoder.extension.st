Extension { #name : 'FilePathEncoder' }

{ #category : '*System-Settings-Browser' }
FilePathEncoder class >> settingInputWidgetForNode: aSettingNode [
	^ aSettingNode  inputWidgetForFileOrDirectoryWithAction: #chooseFilePath
]
