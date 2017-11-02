Extension { #name : #SettingTreeBuilder }

{ #category : #'*System-Settings-Tests' }
SettingTreeBuilder >> mocksystemsettings [
	currentPragma methodClass theNonMetaClass perform: currentPragma methodSelector with: self.
]
