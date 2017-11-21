"
Do not explicitly refer to UITheme current but use Smalltalk ui theme. Similarly do not directly refer to ThemeIcons current but use Smalltalk ui icons.

"
Class {
	#name : #RBThemeAPIUpdateRule,
	#superclass : #ReNodeRewriteRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBThemeAPIUpdateRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ThemeAPIUpdateRule'
]

{ #category : #accessing }
RBThemeAPIUpdateRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBThemeAPIUpdateRule >> initialize [
	super initialize.
	self
		replace: 'UITheme current'
			with: 'Smalltalk ui theme';
		replace: 'ThemeIcons current'
			with: 'Smalltalk ui icons'
]

{ #category : #accessing }
RBThemeAPIUpdateRule >> name [
	^ 'Use "Smalltalk ui theme" and "Smalltalk ui icons"'
]
