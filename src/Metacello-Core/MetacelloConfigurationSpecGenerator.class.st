Class {
	#name : 'MetacelloConfigurationSpecGenerator',
	#superclass : 'MetacelloAbstractSpecGenerator',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloConfigurationSpecGenerator >> projectSpecCreationBlock [
    ^ [ :projectName | {(MetacelloProject new configurationOfProjectSpec name: projectName)} ]
]

{ #category : 'accessing' }
MetacelloConfigurationSpecGenerator >> projectSpecListBlock [
    ^ [ MetacelloProjectRegistration configurationProjectSpecs ]
]

{ #category : 'accessing' }
MetacelloConfigurationSpecGenerator >> projectSpecLookupBlock [
    ^ [ :projectName | 
    {(MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine configurationNameFrom: projectName)
        ifAbsent: [  ])} ]
]
