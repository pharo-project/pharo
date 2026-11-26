Class {
	#name : 'MetacelloGroupSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloGroupSpecTest >> testGroupMergeSpecIncludesMergedGroup [

	| groupCore groupTests group |
	groupCore := self groupSpec
		          name: 'Platform';
		          includes: 'Core';
		          yourself.
	groupTests := self groupSpec
		          name: 'Platform';
		          includes: 'Tests';
		          yourself.
	group := groupCore mergeSpec: groupTests.
	self assert: (group includes includes: 'Core').
	self assert: (group includes includes: 'Tests')
]

{ #category : 'tests' }
MetacelloGroupSpecTest >> testGroupSpecBasicAccessors [

	| group |
	group := self groupSpec
		         name: 'Platform';
		         includes: 'Core';
		         yourself.
	self assert: group name equals: 'Platform'.
	self assert: (group includes includes: 'Core').

]

{ #category : 'tests' }
MetacelloGroupSpecTest >> testGroupSpecIncludesArray [

	| group |
	group := self groupSpec
		         name: 'Platform';
		         includes: #( 'Core' );
		         yourself.
	self assert: group name equals: 'Platform'.
	self assert: (group includes includes: 'Core').
	self assert: group isGroupSpec
]

{ #category : 'tests - errors' }
MetacelloGroupSpecTest >> testGroupSpecRequireEmptyArrayRaiseError [

	| group |
	group := self groupSpec
		         name: 'Platform';
		         includes: #( 'Core' );
		         yourself.
	self should: [ group requires: #(  ) ] raise: Error
]
