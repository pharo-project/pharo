Extension { #name : #String }

{ #category : #'*system-settings-browser' }
String class >> settingInputWidgetForNode: aSettingNode [
	^ aSettingNode inputWidgetForString
]
