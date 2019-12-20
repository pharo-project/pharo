Class {
	#name : #MetacelloCoreSymbolicVersionTest,
	#superclass : #TestCase,
	#category : #'Metacello-TestsCore-Configs'
}

{ #category : #baselines }
MetacelloCoreSymbolicVersionTest >> baseline10: spec [
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'Example Project'
                with: [ 
                    spec
                        className: 'MetacelloExampleProjectConfig';
                        versionString: '1.0-baseline';
                        preLoadDoIt: #'preloadForProject';
                        postLoadDoIt: #'postloadForProject';
                        yourself ] ]
]

{ #category : #baselines }
MetacelloCoreSymbolicVersionTest >> baseline20: spec [
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec blessing: #'baseline'.
            spec
                project: 'Example Project'
                with: [ 
                    spec
                        className: 'MetacelloExampleProjectConfig';
                        versionString: '1.0-baseline';
                        preLoadDoIt: #'preloadForProject';
                        postLoadDoIt: #'postloadForProject';
                        yourself ] ]
]

{ #category : #versions }
MetacelloCoreSymbolicVersionTest >> bleedingEdge: spec [
    <symbolicVersion: #'bleedingEdge'>
    spec for: #'platformVersion1.x' version: '1.0-baseline'
]

{ #category : #accessing }
MetacelloCoreSymbolicVersionTest >> project [

	^self projectWith: #()
]

{ #category : #accessing }
MetacelloCoreSymbolicVersionTest >> projectWith: projectAttributes [
    | project |
    "Construct Metacello project"
    project := MetacelloProject new.
    project projectAttributes: projectAttributes.
    MetacelloVersionConstructor on: self project: project.
    ^ project
]

{ #category : #versions }
MetacelloCoreSymbolicVersionTest >> stableVersion: spec [
    <symbolicVersion: #'stable'>
    spec for: #'platformVersion1.x' version: '1.0'
]

{ #category : #tests }
MetacelloCoreSymbolicVersionTest >> testBasicBleedingEdgeVersion [
	| project version expected actual |
	project := self projectWith: #(#'platformVersion1.x').
	version := project version: #bleedingEdge.
	expected := 'spec blessing: #''baseline''.
spec project: ''Example Project'' with: [
		spec
			className: ''MetacelloExampleProjectConfig'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'' ].'.
	actual := version spec printString.
	self assert: expected equals: actual
]

{ #category : #tests }
MetacelloCoreSymbolicVersionTest >> testBasicStableVersion [
	| project version expected actual |
	project := self projectWith: #(#'platformVersion1.x').
	version := project version: #stable.
	expected := 'spec blessing: #''release''.
spec project: ''Example Project'' with: [
		spec
			className: ''MetacelloExampleProjectConfig'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'' ].'.
	actual := version spec printString.
	self assert: expected equals: actual
]

{ #category : #tests }
MetacelloCoreSymbolicVersionTest >> testBleedingEdgeMethod [
	| project |
	project := self projectWith: #(#'platformVersion1.x').
	self assert: (project version: #bleedingEdge) equals: project bleedingEdge
]

{ #category : #tests }
MetacelloCoreSymbolicVersionTest >> testStableVersionMethod [
	| project |
	project := self projectWith: #(#'platformVersion1.x').
	self assert: (project version: #stable) equals: project stableVersion
]

{ #category : #versions }
MetacelloCoreSymbolicVersionTest >> version10: spec [
    <version: '1.0' imports: #('1.0-baseline')>
    spec for: #'common' do: [ spec blessing: #'release' ]
]
