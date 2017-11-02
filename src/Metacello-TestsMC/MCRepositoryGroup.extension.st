Extension { #name : #MCRepositoryGroup }

{ #category : #'*metacello-testsmc' }
MCRepositoryGroup >> removeIdenticalRepository: aRepository [
  1 to: repositories size do: [ :index | 
    (repositories at: index) == aRepository
      ifTrue: [ 
        repositories removeAt: index.
        ^ self ] ]
]
