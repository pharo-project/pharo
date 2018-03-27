Extension { #name : #SettingTreeBuilder }

{ #category : #'*System-Settings-Tests' }
SettingTreeBuilder >> mocksystemsettings [
	currentPragma methodClass instanceSide perform: currentPragma methodSelector with: self.
]
