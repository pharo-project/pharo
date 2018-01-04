Class {
	#name : #HEReplaceOnDuplication,
	#superclass : #HEDuplicationModeStrategy,
	#category : #'Hermes-Extensions-ClassInstaller'
}

{ #category : #accessing }
HEReplaceOnDuplication class >> keyword [
	^ 'replace'
]

{ #category : #'validating existence' }
HEReplaceOnDuplication >> doExistingClass: aClass hermesClass: aHEClass installer: installer [
	^ nil
]

{ #category : #'validating existence' }
HEReplaceOnDuplication >> doExistingTrait: aTrait hermesTrait: aHETrait installer: installer [
	^ nil
]

{ #category : #'validating existence' }
HEReplaceOnDuplication >> doShouldBuildMethod: aHEMethod in: installer installer: aClass original: aMethod [
	^ true
]
