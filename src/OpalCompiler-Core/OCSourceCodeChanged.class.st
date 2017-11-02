"
An OCSourceCodeChanged is used to pass corrected source code from OCUndeclaredVariableWarning back to OpalCompiler>>compile  so that parsing can be restarted,. Introduced to resolve Case 12954.
"
Class {
	#name : #OCSourceCodeChanged,
	#superclass : #Notification,
	#instVars : [
		'newSourceCode'
	],
	#category : 'OpalCompiler-Core-Exception'
}

{ #category : #accessing }
OCSourceCodeChanged >> newSourceCode [
	^newSourceCode
]

{ #category : #accessing }
OCSourceCodeChanged >> newSourceCode: aString [
	newSourceCode := aString
]
