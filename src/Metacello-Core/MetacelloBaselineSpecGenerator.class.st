Class {
	#name : 'MetacelloBaselineSpecGenerator',
	#superclass : 'MetacelloProjectSpecGenerator',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloBaselineSpecGenerator >> projectSpecCreationBlock [
    ^ [ :projectName | {(MetacelloMCBaselineProject new baselineOfProjectSpec name: projectName)} ]
]

{ #category : 'accessing' }
MetacelloBaselineSpecGenerator >> projectSpecListBlock [
    ^ [ MetacelloProjectRegistration baselineProjectSpecs ]
]

{ #category : 'accessing' }
MetacelloBaselineSpecGenerator >> projectSpecLookupBlock [
    ^ [ :projectName | 
    {(MetacelloProjectRegistration
        projectSpecForClassNamed: (MetacelloScriptEngine baselineNameFrom: projectName)
        ifAbsent: [  ])} ]
]
