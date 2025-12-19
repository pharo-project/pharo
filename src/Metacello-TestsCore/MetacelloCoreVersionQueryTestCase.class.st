Class {
	#name : 'MetacelloCoreVersionQueryTestCase',
	#superclass : 'TestCase',
	#category : 'Metacello-TestsCore-Configs',
	#package : 'Metacello-TestsCore',
	#tag : 'Configs'
}

{ #category : 'baselines' }
MetacelloCoreVersionQueryTestCase >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec baseline: 'UTF8' with: [
					spec
						repository: 'repository';
						loads: #( 'Misc-UTF8' ) ].
			spec
				group: 'Core' with: #( 'UTF8' );
				group: 'Tests' with: #( 'UTF8' );
				group: 'Recursive' with: #( 'Core' 'Recursive' ) "recursive group definition" ]
]

{ #category : 'accessing' }
MetacelloCoreVersionQueryTestCase >> project [

	^ (MetacelloBaselineConstructor on: self) project
]

{ #category : 'tests' }
MetacelloCoreVersionQueryTestCase >> testBaselineGroups [

	| project expected list |
	project := self project.
	expected := #( 'Core' 'Tests' 'Recursive' ).
	list := project version groups.
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ]
]

{ #category : 'tests' }
MetacelloCoreVersionQueryTestCase >> testBaselineProjects [

	| project expected list |
	project := self project.
	expected := #( 'UTF8' ).
	list := project version projects.
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ]
]
