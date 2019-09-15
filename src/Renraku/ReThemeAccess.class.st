"
Morph and Model subclasses methods should access GUI theme via 'self theme' and not 'Smalltalk ui theme'.
"
Class {
	#name : #ReThemeAccess,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReThemeAccess class >> checksMethod [
	^ true
]

{ #category : #running }
ReThemeAccess >> basicCheck: aMethod [
	^ ((aMethod methodClass inheritsFrom: Morph) 
		or: [aMethod methodClass inheritsFrom: Model])
	and: [aMethod sourceCode includesAll: 'Smalltalk ui theme']

]

{ #category : #running }
ReThemeAccess >> group [
	^ 'API Hints'
]

{ #category : #running }
ReThemeAccess >> name [
	^ 'Access to UI theme.'
]

{ #category : #running }
ReThemeAccess >> severity [
	^ #warning
]
