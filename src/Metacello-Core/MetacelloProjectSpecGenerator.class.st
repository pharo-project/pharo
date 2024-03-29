Class {
	#name : 'MetacelloProjectSpecGenerator',
	#superclass : 'Object',
	#instVars : [
		'target'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> projectSpecCreationBlock [
    ^ [ :projectName | {(MetacelloProject new projectSpec name: projectName)} ]
]

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> projectSpecListBlock [
    ^ [ MetacelloProjectRegistration projectSpecs ]
]

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> projectSpecLookupBlock [
    ^ [ :projectName | 
    {(MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine baselineNameFrom: projectName)
        ifAbsent: [  ]).
    (MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine configurationNameFrom: projectName)
        ifAbsent: [  ])} ]
]

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> target [
	^ target
]

{ #category : 'accessing' }
MetacelloProjectSpecGenerator >> target: anObject [
	target := anObject
]
