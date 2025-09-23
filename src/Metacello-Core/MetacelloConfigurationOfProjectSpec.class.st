Class {
	#name : 'MetacelloConfigurationOfProjectSpec',
	#superclass : 'MetacelloProjectSpec',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'scripting' }
MetacelloConfigurationOfProjectSpec >> asConfigurationProjectSpec [
    ^ self
]

{ #category : 'scripting' }
MetacelloConfigurationOfProjectSpec >> asProjectRegistration [
    ^ MetacelloProjectRegistration fromMCConfigurationProjectSpec: self
]

{ #category : 'private' }
MetacelloConfigurationOfProjectSpec >> constructClassName [
    ^ 'ConfigurationOf' , self name
]

{ #category : 'mutability' }
MetacelloConfigurationOfProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
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

{ #category : 'printing' }
MetacelloConfigurationOfProjectSpec >> hasClassName [

	^ className isNotNil and: [ className ~= self constructClassName ]
]

{ #category : 'testing' }
MetacelloConfigurationOfProjectSpec >> hasConflictWithBaselineSpec: projectSpec [
	projectSpec name = self name
		ifFalse: [ ^ true ].
	projectSpec project configuration className = self project configuration className
		ifFalse: [ ^ true ].
	^ ((projectSpec repositories isEmpty or: [ self repositories isEmpty ])
		or: [ projectSpec repositories hasNoLoadConflicts: self repositories ]) not
]

{ #category : 'testing' }
MetacelloConfigurationOfProjectSpec >> hasConflictWithProjectSpec: projectSpec [
	^ projectSpec hasConflictWithConfigurationSpec: self
]

{ #category : 'testing' }
MetacelloConfigurationOfProjectSpec >> isConfigurationOfProjectSpec [
	^ true
]

{ #category : 'merging' }
MetacelloConfigurationOfProjectSpec >> mergeSpec: anotherSpec [
    ^ super mergeSpec: anotherSpec asConfigurationProjectSpec
]

{ #category : 'printing' }
MetacelloConfigurationOfProjectSpec >> projectLabel [
    ^ 'configuration'
]
