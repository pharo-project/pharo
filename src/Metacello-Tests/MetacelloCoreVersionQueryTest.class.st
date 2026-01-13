Class {
	#name : 'MetacelloCoreVersionQueryTest',
	#superclass : 'TestCase',
	#category : 'Metacello-Tests-Configs',
	#package : 'Metacello-Tests',
	#tag : 'Configs'
}

{ #category : 'baselines' }
MetacelloCoreVersionQueryTest >> baseline: spec [

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
MetacelloCoreVersionQueryTest >> project [

	^ (MetacelloBaselineConstructor on: self) project
]

{ #category : 'tests' }
MetacelloCoreVersionQueryTest >> testBaselineGroups [

	| project expected list |
	project := self project.
	expected := #( 'Core' 'Tests' 'Recursive' ).
	list := project version groups.
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ]
]

{ #category : 'tests' }
MetacelloCoreVersionQueryTest >> testBaselineProjects [

	| project expected list |
	project := self project.
	expected := #( 'UTF8' ).
	list := project version projects.
	self assert: list size equals: expected size.
	list do: [ :pkg | self assert: (expected includes: pkg name) ]
]
