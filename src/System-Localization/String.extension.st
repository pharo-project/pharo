Extension { #name : #String }

{ #category : #'*System-Localization' }
String >> translated [
"answer the receiver translated to the default language"
	^ NaturalLanguageTranslator translate: self

]

{ #category : #'*System-Localization' }
String >> translatedTo: localeID [ 
	"answer the receiver translated to the given locale id"
	^ NaturalLanguageTranslator translate: self toLocale: localeID
]
