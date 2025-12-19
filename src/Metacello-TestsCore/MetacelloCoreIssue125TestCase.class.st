Class {
	#name : 'MetacelloCoreIssue125TestCase',
	#superclass : 'TestCase',
	#category : 'Metacello-TestsCore-Configs',
	#package : 'Metacello-TestsCore',
	#tag : 'Configs'
}

{ #category : 'baselines' }
MetacelloCoreIssue125TestCase >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				preLoadDoIt: #preloadForCore;
				postLoadDoIt: #postloadForCore:package:.
			spec baseline: 'Example Project' with: [
					spec
						repository: 'repository';
						preLoadDoIt: #preloadForProject;
						postLoadDoIt: #postloadForProject;
						loads: #( 'core' ) ].

			spec group: 'Core' with: #( 'Example Project' ) ].
	spec for: #( #attribute1 #attribute2 ) do: [
			spec baseline: 'Extra Project' with: [
					spec
						repository: 'repository2';
						preLoadDoIt: #preloadForProject;
						postLoadDoIt: #postloadForProject;
						loads: #( 'core' ) ].
			spec group: 'Core' with: #( 'Extra Project' ) ]
]

{ #category : 'accessing' }
MetacelloCoreIssue125TestCase >> projectWith: projectAttributes [

	| project |
	"Construct Metacello project"
	project := MetacelloBaselineProject new.
	project projectAttributes: projectAttributes.
	MetacelloBaselineConstructor on: self project: project.
	^ project
]

{ #category : 'tests' }
MetacelloCoreIssue125TestCase >> testForDoListAtrribute1Active [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project expected |
	project := self projectWith: #( #attribute1 ).
	expected := 'spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec 
	baseline: ''Example Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository'' ];
	baseline: ''Extra Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository2'' ].
spec 
	group: ''Core'' with: #(''Example Project'' );
	group: ''Core'' with: #(''Extra Project'' ).'.
	self assert: expected equals: project version spec printString
]

{ #category : 'tests' }
MetacelloCoreIssue125TestCase >> testForDoListAtrribute2Active [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project expected |
	project := self projectWith: #( #attribute2 ).
	expected := 'spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec 
	baseline: ''Example Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository'' ];
	baseline: ''Extra Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository2'' ].
spec 
	group: ''Core'' with: #(''Example Project'' );
	group: ''Core'' with: #(''Extra Project'' ).'.
	self assert: expected equals: project version spec printString
]

{ #category : 'tests' }
MetacelloCoreIssue125TestCase >> testForDoListAttribute1Atrribute2Active [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project expected |
	project := self projectWith: #( #attribute1 #attribute2 ).
	expected := 'spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec 
	baseline: ''Example Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository'' ];
	baseline: ''Extra Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository2'' ];
	baseline: ''Extra Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository2'' ].
spec 
	group: ''Core'' with: #(''Example Project'' );
	group: ''Core'' with: #(''Extra Project'' );
	group: ''Core'' with: #(''Extra Project'' ).'.
	self assert: expected equals: project version spec printString
]

{ #category : 'tests' }
MetacelloCoreIssue125TestCase >> testForDoListNotActive [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project expected |
	project := self projectWith: #(  ).
	expected := 'spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec baseline: ''Example Project'' with: [
		spec
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' );
			repository: ''repository'' ].
spec group: ''Core'' with: #(''Example Project'' ).'.
	self assert: expected equals: project version spec printString
]
