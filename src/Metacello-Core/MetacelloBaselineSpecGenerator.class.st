Class {
	#name : 'MetacelloBaselineSpecGenerator',
	#superclass : 'MetacelloProjectSpecGenerator',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloBaselineSpecGenerator >> projectSpecCreationBlock [

	^ [ :projectName |
	  | projectSpec spec |
	  projectSpec := MetacelloMCBaselineProject new baselineOfProjectSpec
		                 name: projectName;
		                 className: 'BaselineOf' , projectName;
		                 yourself.
	  spec := MetacelloMCBaselineProject new projectReferenceSpec
		          name: projectName;
		          projectReference: projectSpec;
		          yourself.
	  { spec } ]
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
