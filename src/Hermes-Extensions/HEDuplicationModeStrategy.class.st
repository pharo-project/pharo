Class {
	#name : #HEDuplicationModeStrategy,
	#superclass : #Object,
	#category : #'Hermes-Extensions-ClassInstaller'
}

{ #category : #'as yet unclassified' }
HEDuplicationModeStrategy class >> forOption: keyword [
	^ (self allSubclasses
		detect: [ :e | e keyword = keyword ]
		ifNone: [ self error: 'There is no duplication strategy for ''' , keyword , '''' ]) new
]

{ #category : #'validating existence' }
HEDuplicationModeStrategy >> existingClass: aHEClass on: installer [
	^ installer environment
		at: aHEClass className
		ifPresent: [ :aClass | 
			self 
				doExistingClass: aClass 
				hermesClass: aHEClass 
				installer: installer  ]
		ifAbsent: [ nil ]
]

{ #category : #'validating existence' }
HEDuplicationModeStrategy >> existingTrait: aHETrait on: installer [
	^ installer environment
		at: aHETrait traitName
		ifPresent: [ :aTrait | 
			self 
				doExistingTrait: aTrait 
				hermesTrait: aHETrait 
				installer: installer ]
		ifAbsent: [ nil ]
]

{ #category : #'validating existence' }
HEDuplicationModeStrategy >> shouldBuildMethod: aHEMethod in: aClass installer: installer [
	^ aClass
		compiledMethodAt: aHEMethod name
		ifPresent: [ :m | 
			(m isFromTrait and: [ aClass isTrait not ])
				ifTrue: [ ^ true ].
			self 
				doShouldBuildMethod: aHEMethod 
				in: installer 
				installer: aClass 
				original: m]
		ifAbsent: [ true ]
]
