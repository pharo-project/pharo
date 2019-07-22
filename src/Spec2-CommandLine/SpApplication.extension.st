Extension { #name : #SpApplication }

{ #category : #'*Spec2-CommandLine' }
SpApplication class >> applicationName [

	^ nil
]

{ #category : #'*Spec2-CommandLine' }
SpApplication class >> findAll [

	^ self allSubclasses select: [ :each | each applicationName notNil ] 
]

{ #category : #'*Spec2-CommandLine' }
SpApplication class >> findNamed: aString ifNone: aBlock [

	^ self allSubclasses 
		detect: [ :each | each applicationName = aString ]
		ifNone: aBlock
]

{ #category : #'*Spec2-CommandLine' }
SpApplication >> prepareAsStandaloneApplication [
	"override this to specify  standalone settings"
]
