Class {
	#name : #SpApplicationWithLocaleTest,
	#superclass : #TestCase,
	#category : #'Spec2-Morphic-Backend-Tests-Localization'
}

{ #category : #tests }
SpApplicationWithLocaleTest >> testLabel [
	| app label string |
	app := SpTestApplicationWithLocale new.
	app locale: (Locale isoLanguage: 'en' isoCountry: 'US').
	app useBackend: #Morphic.
	string := SpTestLocalizedString from: 'string'.
	label := app newPresenter: SpLabelPresenter.
	label label: string.	
	label openWithSpec.
	self assert: label adapter widget contents equals: 'string[en-US](1)'.

	app locale: (Locale isoLanguage: 'de' isoCountry: 'DE').
	label localeChanged.
	self assert: label adapter widget contents equals: 'string[de-DE](2)'.
	
	label window close.
]
