Class {
	#name : #MetacelloCoreVersionQueryTestCase,
	#superclass : #TestCase,
	#category : #'Metacello-TestsCore-Configs'
}

{ #category : #baselines }
MetacelloCoreVersionQueryTestCase >> baseline10: spec [
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'UTF8'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryMiscConfig';
                        versionString: '1.0-baseline';
                        loads: #('Misc-UTF8') ].
            spec
                group: 'Core' with: #('UTF8');
                group: 'Tests' with: #('UTF8');
                group: 'Recursive' with: #('Core' 'Recursive')	"recursive group definition" ]
]

{ #category : #baselines }
MetacelloCoreVersionQueryTestCase >> baseline11: spec [
    <version: '1.1-baseline' imports: #('1.0-baseline')>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'UTF7'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryMiscConfig';
                        versionString: '1.0-baseline';
                        loads: #('Misc-UTF7') ].
            spec
                group: 'Core' with: #('UTF7');
                group: 'Tests' with: #('UTF7');
                yourself ]
]

{ #category : #baselines }
MetacelloCoreVersionQueryTestCase >> baseline20: spec [
    <version: '2.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                project: 'UTF8'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryMiscConfig';
                        versionString: #'stable';
                        loads: #('Misc-UTF8') ].
            spec
                project: 'UTF9'
                with: [ 
                    spec
                        className: 'MetacelloVersionQueryOtherConfig';
                        versionString: '1.0-baseline';
                        loads: #('Misc-UTF8') ].
            spec
                group: 'Core' with: #('UTF8' 'UTF9');
                group: 'Tests' with: #('UTF8');
                group: 'Recursive' with: #('Core' 'Recursive')	"recursive group definition" ]
]

{ #category : #accessing }
MetacelloCoreVersionQueryTestCase >> project [

	^self projectWith: #()
]

{ #category : #accessing }
MetacelloCoreVersionQueryTestCase >> projectWith: projectAttributes [
    | project |
    "Construct Metacello project"
    project := MetacelloProject new.
    project projectAttributes: projectAttributes.
    MetacelloVersionConstructor on: self project: project.
    ^ project
]

{ #category : #tests }
MetacelloCoreVersionQueryTestCase >> testBaseline10Groups [
	| project version expected list |
	project := self project.
	version := project version: '1.0-baseline'.
	expected := #('Core' 'Tests' 'Recursive').
	list := version groups.
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ]
]

{ #category : #tests }
MetacelloCoreVersionQueryTestCase >> testBaseline10Projects [
	| project version expected list |
	project := self project.
	version := project version: '1.0-baseline'.
	expected := #('UTF8').
	list := version projects.
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ]
]

{ #category : #tests }
MetacelloCoreVersionQueryTestCase >> testBaseline10ResolveToLoadableSpecs01 [
	| project version expected list |
	project := self project.
	version := project version: '1.0-baseline'.
	expected := #('UTF8').
	list := version resolveToLoadableSpecs: #('UTF8').
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ].
	expected := #('UTF8').
	list := version resolveToLoadableSpecs: #('Recursive').
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ]
]

{ #category : #tests }
MetacelloCoreVersionQueryTestCase >> testDifference [
	| project expected fromVersionString toVersionString report actual |
	project := self project.
	fromVersionString := '1.1-baseline'.
	toVersionString := '2.0-baseline'.
	report := (project version: fromVersionString) difference: (project version: toVersionString).
	report
		from: fromVersionString;
		to: toVersionString;
		configuration: 'Test Configuration'.
	expected := 'Test Configuration ''1.1-baseline'' to ''2.0-baseline''
	Additions:
		UTF9
			'''' to ''1.0-baseline''
	Modifications:
		UTF8
			''1.0-baseline'' to ''stable''
	Removals:
		UTF7
			''1.0-baseline'' to ''''
'.
	actual := report printString.
	self assert: expected equals: actual
]
