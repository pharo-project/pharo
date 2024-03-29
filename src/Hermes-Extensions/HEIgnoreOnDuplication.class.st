"
When there is a duplication, I do not do nothing. I keep the version already in the image
"
Class {
	#name : 'HEIgnoreOnDuplication',
	#superclass : 'HEDuplicationModeStrategy',
	#category : 'Hermes-Extensions-ClassInstaller',
	#package : 'Hermes-Extensions',
	#tag : 'ClassInstaller'
}

{ #category : 'accessing' }
HEIgnoreOnDuplication class >> keyword [
	^ 'ignore'
]

{ #category : 'validating existence' }
HEIgnoreOnDuplication >> doExistingClass: aClass hermesClass: aHEClass installer: installer [
	^ aClass
]

{ #category : 'validating existence' }
HEIgnoreOnDuplication >> doExistingTrait: aTrait hermesTrait: aHETrait installer: installer [
	^ aTrait
]

{ #category : 'validating existence' }
HEIgnoreOnDuplication >> doShouldBuildMethod: aHEMethod in: installer installer: aClass original: aMethod [
	^ false
]
