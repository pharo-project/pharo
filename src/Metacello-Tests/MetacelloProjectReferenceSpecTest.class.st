Class {
	#name : 'MetacelloProjectReferenceSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloProjectReferenceSpecTest >> testProjectReferenceMergeSpec [

	| projectReferenceA projectReferenceB projectReference project |
	projectReferenceA := MetacelloProjectReferenceSpec projectReference: (self projectSpec
			                      name: 'Project';
			                      className: 'BaselinenOfProjectA';
			                      yourself).
	projectReferenceB := MetacelloProjectReferenceSpec projectReference: (self projectSpec
			                      name: 'Project';
			                      className: 'BaselineOfProjectB';
			                      yourself).
	projectReference := projectReferenceA mergeSpec: projectReferenceB.
	self assert: projectReference name equals: 'Project'.
	self assert: projectReference projectName equals: 'Project'.
	project := projectReference projectReference.
	self assert: project className equals: 'BaselineOfProjectB'
]

{ #category : 'tests' }
MetacelloProjectReferenceSpecTest >> testProjectReferenceSpec [

	| projectReference project |
	projectReference := MetacelloProjectReferenceSpec projectReference: (project := self projectSpec
			                                name: 'Project';
			                                yourself).
	self assert: projectReference name equals: 'Project'.
	self assert: projectReference projectName equals: 'Project'.
	self assert: projectReference projectReference identicalTo: project.
	self should: [ projectReference includes: #(  ) ] raise: Error.
	self should: [ projectReference requires: #(  ) ] raise: Error.
	self assert: projectReference isProjectReferenceSpec
]
