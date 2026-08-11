Extension { #name : 'SettingTreeBuilder' }

{ #category : '*System-Settings-Tests' }
SettingTreeBuilder >> mocksystemsettings [

	self currentPragma methodClass instanceSide perform: self currentPragma methodSelector with: self
]
