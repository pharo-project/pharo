Class {
	#name : 'MetacelloBaselineSpecGenerator',
	#superclass : 'MetacelloAbstractSpecGenerator',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloBaselineSpecGenerator >> projectSpecCreationBlock [

	^ [ :projectName |
	  | projectSpec spec |
	  projectSpec := MetacelloBaselineProject new baselineOfProjectSpec
		                 name: projectName;
		                 className: 'BaselineOf' , projectName;
		                 yourself.
	  spec := MetacelloBaselineProject new projectReferenceSpec
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
