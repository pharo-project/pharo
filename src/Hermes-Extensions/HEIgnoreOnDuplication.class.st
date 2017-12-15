Class {
	#name : #HEIgnoreOnDuplication,
	#superclass : #HEDuplicationModeStrategy,
	#category : #'Hermes-Extensions-ClassInstaller'
}

{ #category : #accessing }
HEIgnoreOnDuplication class >> keyword [
	^ 'ignore'
]

{ #category : #'validating existence' }
HEIgnoreOnDuplication >> doExistingClass: aClass hermesClass: aHEClass installer: installer [
	^ aClass
]

{ #category : #'validating existence' }
HEIgnoreOnDuplication >> doExistingTrait: aTrait hermesTrait: aHETrait installer: installer [
	^ aTrait
]

{ #category : #'validating existence' }
HEIgnoreOnDuplication >> doShouldBuildMethod: aHEMethod in: installer installer: aClass original: aMethod [
	^ false
]
