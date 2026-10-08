Extension { #name : 'String' }

{ #category : '*System-Localization' }
String >> translated [
	"answer the receiver translated to the default language"
	self deprecated: 'translated is not offered anymore please use more standard way such as https://github.com/astares/Pharo-I18N'.
	^  self
]
