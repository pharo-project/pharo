"
Morph and Model subclasses methods should access GUI theme via 'self theme' and not 'Smalltalk ui theme'.
"
Class {
	#name : #ReThemeAccess,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'as yet unclassified' }
ReThemeAccess class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReThemeAccess >> basicCheck: aMethod [
	^ ((aMethod methodClass inheritsFrom: Morph) 
		or: [aMethod methodClass inheritsFrom: Model])
	and: [aMethod sourceCode includesAll: 'Smalltalk ui theme']

]

{ #category : #accessing }
ReThemeAccess >> group [
	^ 'API Hints'
]

{ #category : #accessing }
ReThemeAccess >> name [
	^ 'Access to UI theme.'
]

{ #category : #accessing }
ReThemeAccess >> severity [
	^ #warning
]
