"
Do not explicitly refer to UITheme current but use Smalltalk ui theme. Similarly do not directly refer to ThemeIcons current but use Smalltalk ui icons.

"
Class {
	#name : #ReThemeAPIUpdateRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReThemeAPIUpdateRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ThemeAPIUpdateRule'
]

{ #category : #accessing }
ReThemeAPIUpdateRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReThemeAPIUpdateRule >> initialize [
	super initialize.
	self
		replace: 'UITheme current'
			with: 'Smalltalk ui theme';
		replace: 'ThemeIcons current'
			with: 'Smalltalk ui icons'
]

{ #category : #accessing }
ReThemeAPIUpdateRule >> name [
	^ 'Use "Smalltalk ui theme" and "Smalltalk ui icons"'
]
