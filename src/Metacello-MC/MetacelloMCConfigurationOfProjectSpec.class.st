Class {
	#name : #MetacelloMCConfigurationOfProjectSpec,
	#superclass : #MetacelloMCProjectSpec,
	#category : #Metacello-MC-Specs
}

{ #category : #scripting }
MetacelloMCConfigurationOfProjectSpec >> asConfigurationProjectSpec [
    ^ self
]

{ #category : #scripting }
MetacelloMCConfigurationOfProjectSpec >> asProjectRegistration [
    ^ MetacelloProjectRegistration fromMCConfigurationProjectSpec: self
]

{ #category : #private }
MetacelloMCConfigurationOfProjectSpec >> constructClassName [
    ^ 'ConfigurationOf' , self name
]

{ #category : #mutability }
MetacelloMCConfigurationOfProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
  | copy |
  aMetacelloProjectRegistration
    configurationProjectSpecIfPresent: [ :spec | 
      copy := spec copy.
      aBlock value: copy.
      aMetacelloProjectRegistration configurationProjectSpec: copy ]
    ifAbsent: [ 
      aMetacelloProjectRegistration
        baselineProjectSpecIfPresent: [ :spec | 
          copy := spec copy.
          aBlock value: copy.
          aMetacelloProjectRegistration baselineProjectSpec: copy ]
        ifAbsent: [ aBlock value: nil ] ]
]

{ #category : #printing }
MetacelloMCConfigurationOfProjectSpec >> hasClassName [
    ^ className ~~ nil and: [ className ~= self constructClassName ]
]

{ #category : #testing }
MetacelloMCConfigurationOfProjectSpec >> hasConflictWithBaselineSpec: projectSpec [
	projectSpec name = self name
		ifFalse: [ ^ true ].
	projectSpec project configuration className = self project configuration className
		ifFalse: [ ^ true ].
	^ ((projectSpec repositories isEmpty or: [ self repositories isEmpty ])
		or: [ projectSpec repositories hasNoLoadConflicts: self repositories ]) not
]

{ #category : #testing }
MetacelloMCConfigurationOfProjectSpec >> hasConflictWithProjectSpec: projectSpec [
	^ projectSpec hasConflictWithConfigurationSpec: self
]

{ #category : #testing }
MetacelloMCConfigurationOfProjectSpec >> isConfigurationOfProjectSpec [
	^ true
]

{ #category : #merging }
MetacelloMCConfigurationOfProjectSpec >> mergeSpec: anotherSpec [
    ^ super mergeSpec: anotherSpec asConfigurationProjectSpec
]

{ #category : #printing }
MetacelloMCConfigurationOfProjectSpec >> projectLabel [
    ^ 'configuration'
]
