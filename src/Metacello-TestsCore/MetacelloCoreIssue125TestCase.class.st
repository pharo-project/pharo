Class {
	#name : #MetacelloCoreIssue125TestCase,
	#superclass : #TestCase,
	#category : #'Metacello-TestsCore-Configs'
}

{ #category : #baselines }
MetacelloCoreIssue125TestCase >> baseline200: spec [
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                preLoadDoIt: #'preloadForCore';
                postLoadDoIt: #'postloadForCore:package:'.
            spec
                project: 'Example Project'
                    with: [ 
                            spec
                                className: 'ConfigurationOfExampleProject';
                                versionString: '1.0-baseline';
                                preLoadDoIt: #'preloadForProject';
                                postLoadDoIt: #'postloadForProject';
                                loads: #('core');
                                yourself ];
                yourself.
            spec group: 'Core' with: #('Example Project') ]
]

{ #category : #baselines }
MetacelloCoreIssue125TestCase >> baseline300: spec [
    <version: '3.0-baseline' imports: #('2.0-baseline')>
    spec
        for: #(#'attribute1' #'attribute2')
        do: [ 
            spec
                project: 'Extra Project'
                    with: [ 
                            spec
                                className: 'ConfigurationOfExtraProject';
                                versionString: '1.0-baseline';
                                preLoadDoIt: #'preloadForProject';
                                postLoadDoIt: #'postloadForProject';
                                loads: #('core');
                                yourself ];
                yourself.
            spec group: 'Core' with: #('Extra Project') ]
]

{ #category : #accessing }
MetacelloCoreIssue125TestCase >> project [

	^self projectWith: #()
]

{ #category : #accessing }
MetacelloCoreIssue125TestCase >> projectWith: projectAttributes [
    | project |
    "Construct Metacello project"
    project := MetacelloProject new.
    project projectAttributes: projectAttributes.
    MetacelloVersionConstructor on: self project: project.
    ^ project
]

{ #category : #tests }
MetacelloCoreIssue125TestCase >> testForDoListAtrribute1Active [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project version expected actual |
	project := self projectWith: #(#attribute1).
	version := project version: '3.0-baseline'.
	expected := 'spec blessing: #''baseline''.
spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec 
	project: ''Example Project'' with: [
		spec
			className: ''ConfigurationOfExampleProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ];
	project: ''Extra Project'' with: [
		spec
			className: ''ConfigurationOfExtraProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ].
spec 
	group: ''Core'' with: #(''Example Project'' );
	group: ''Core'' with: #(''Extra Project'' ).'.
	actual := version spec printString.
	self assert: expected equals: actual
]

{ #category : #tests }
MetacelloCoreIssue125TestCase >> testForDoListAtrribute2Active [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project version expected actual |
	project := self projectWith: #(#attribute2).
	version := project version: '3.0-baseline'.
	expected := 'spec blessing: #''baseline''.
spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec 
	project: ''Example Project'' with: [
		spec
			className: ''ConfigurationOfExampleProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ];
	project: ''Extra Project'' with: [
		spec
			className: ''ConfigurationOfExtraProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ].
spec 
	group: ''Core'' with: #(''Example Project'' );
	group: ''Core'' with: #(''Extra Project'' ).'.
	actual := version spec printString.
	self assert: expected equals: actual
]

{ #category : #tests }
MetacelloCoreIssue125TestCase >> testForDoListAttribute1Atrribute2Active [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project version expected actual |
	project := self projectWith: #(#attribute1 #attribute2).
	version := project version: '3.0-baseline'.
	expected := 'spec blessing: #''baseline''.
spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec 
	project: ''Example Project'' with: [
		spec
			className: ''ConfigurationOfExampleProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ];
	project: ''Extra Project'' with: [
		spec
			className: ''ConfigurationOfExtraProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ];
	project: ''Extra Project'' with: [
		spec
			className: ''ConfigurationOfExtraProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ].
spec 
	group: ''Core'' with: #(''Example Project'' );
	group: ''Core'' with: #(''Extra Project'' );
	group: ''Core'' with: #(''Extra Project'' ).'.
	actual := version spec printString.
	self assert: expected equals: actual
]

{ #category : #tests }
MetacelloCoreIssue125TestCase >> testForDoListNotActive [
	"https://github.com/dalehenrich/metacello-work/issues/125"

	| project version expected actual |
	project := self projectWith: #().
	version := project version: '3.0-baseline'.
	expected := 'spec blessing: #''baseline''.
spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec project: ''Example Project'' with: [
		spec
			className: ''ConfigurationOfExampleProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ].
spec group: ''Core'' with: #(''Example Project'' ).'.
	actual := version spec printString.
	self assert: expected equals: actual
]
