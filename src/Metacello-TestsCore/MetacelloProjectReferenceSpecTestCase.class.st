Class {
	#name : 'MetacelloProjectReferenceSpecTestCase',
	#superclass : 'MetacelloSpecTestCase',
	#category : 'Metacello-TestsCore-Specs',
	#package : 'Metacello-TestsCore',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloProjectReferenceSpecTestCase >> testProjectReferenceMergeSpec [

	| projectReferenceA projectReferenceB projectReference project |
	projectReferenceA := self projectReferenceSpec
		                     name: 'Project';
		                     projectReference: (self projectSpec
				                      name: 'Project';
				                      className: 'BaselineOfProjectA';
				                      yourself);
		                     yourself.
	projectReferenceB := self projectReferenceSpec
		                     name: 'Project';
		                     projectReference: (self projectSpec
				                      name: 'Project';
				                      className: 'BaselineOfProjectB';
				                      yourself);
		                     yourself.
	projectReference := projectReferenceA mergeSpec: projectReferenceB.
	self assert: projectReference name equals: 'Project'.
	self assert: projectReference projectName equals: 'Project'.
	project := projectReference projectReference.
	self assert: project className equals: 'BaselineOfProjectB'
]

{ #category : 'tests' }
MetacelloProjectReferenceSpecTestCase >> testProjectReferenceSpec [
	| projectReference project |
	projectReference := self projectReferenceSpec
		name: 'Project';
		projectReference:
			(project := self projectSpec
				name: 'Project';
				className: 'BaselineOfProjectA';
				yourself);
		yourself.
	self assert: projectReference name equals: 'Project'.
	self assert: projectReference projectName equals: 'Project'.
	self assert: projectReference projectReference identicalTo: project.
	self should: [ projectReference includes: #() ] raise: Error.
	self should: [ projectReference requires: #() ] raise: Error.
	self should: [ projectReference answers: #() ] raise: Error.
	projectReference
		projectDo: [ :prjct | self assert: projectReference identicalTo: prjct ]
		packageDo: [ :ignored | self assert: false ]
		groupDo: [ :ignored | self assert: false ]
]
