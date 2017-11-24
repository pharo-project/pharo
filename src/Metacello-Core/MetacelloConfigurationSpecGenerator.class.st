Class {
	#name : #MetacelloConfigurationSpecGenerator,
	#superclass : #MetacelloProjectSpecGenerator,
	#category : #'Metacello-Core-Scripts'
}

{ #category : #accessing }
MetacelloConfigurationSpecGenerator >> projectSpecCreationBlock [
    ^ [ :projectName | {(MetacelloMCProject new configurationOfProjectSpec name: projectName)} ]
]

{ #category : #accessing }
MetacelloConfigurationSpecGenerator >> projectSpecListBlock [
    ^ [ MetacelloProjectRegistration configurationProjectSpecs ]
]

{ #category : #accessing }
MetacelloConfigurationSpecGenerator >> projectSpecLookupBlock [
    ^ [ :projectName | 
    {(MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine configurationNameFrom: projectName)
        ifAbsent: [  ])} ]
]
