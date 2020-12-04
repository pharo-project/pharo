Class {
	#name : #SpTestLocalizedString,
	#superclass : #Object,
	#instVars : [
		'string',
		'requestsCount'
	],
	#category : #'Spec2-Tests-Localization'
}

{ #category : #'instance creation' }
SpTestLocalizedString class >> from: aString [

	^ self new string: aString
]

{ #category : #accessing }
SpTestLocalizedString >> asString [ 
	^ string asString
]

{ #category : #accessing }
SpTestLocalizedString >> initialize [ 

	super initialize.
	requestsCount := 0
]

{ #category : #accessing }
SpTestLocalizedString >> isEmptyOrNil [

	^ self string isEmpty
]

{ #category : #accessing }
SpTestLocalizedString >> localizedForPresenter: aPresenter [ 

	requestsCount := requestsCount + 1.
	
	^ string, '[', aPresenter locale localeID isoString, '](', requestsCount asString, ')'
]

{ #category : #accessing }
SpTestLocalizedString >> string [
	^ string
]

{ #category : #accessing }
SpTestLocalizedString >> string: anObject [
	string := anObject
]

{ #category : #accessing }
SpTestLocalizedString >> withAccentuatedCharacter: aCharacter [


]
