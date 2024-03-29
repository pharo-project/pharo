"
I implement the default behavior.
Producing an error if there is a duplication.
"
Class {
	#name : 'HEFailOnDuplication',
	#superclass : 'HEDuplicationModeStrategy',
	#category : 'Hermes-Extensions-ClassInstaller',
	#package : 'Hermes-Extensions',
	#tag : 'ClassInstaller'
}

{ #category : 'accessing' }
HEFailOnDuplication class >> keyword [
	^ 'fail'
]

{ #category : 'validating existence' }
HEFailOnDuplication >> doExistingClass: aClass hermesClass: aHEClass installer: installer [
	^ self error: (installer messageExistingClass: aHEClass)
]

{ #category : 'validating existence' }
HEFailOnDuplication >> doExistingTrait: aTrait hermesTrait: aHETrait installer: installer [
	^ self error: (installer messageExistingTrait: aHETrait)
]

{ #category : 'validating existence' }
HEFailOnDuplication >> doShouldBuildMethod: aHEMethod in: installer installer: aClass original: aMethod [
	^ self error: (installer messageMethod: aHEMethod alreadyExistsIn: aClass)
]
