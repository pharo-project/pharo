Class {
	#name : 'MetacelloGroupSpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloGroupSpecTest >> testGroupMergeSpec [
    | groupA groupB group |
    groupA := self groupSpec
        name: 'Platform';
        includes: 'Core';
        yourself.
    groupB := self groupSpec
        name: 'Platform';
        includes: 'Tests';
        yourself.
    group := groupA mergeSpec: groupB.
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests')
]

{ #category : 'tests' }
MetacelloGroupSpecTest >> testGroupSpec [

	| group |
	group := self groupSpec
		         name: 'Platform';
		         includes: 'Core';
		         yourself.
	self assert: group name equals: 'Platform'.
	self assert: (group includes includes: 'Core').
	group := self groupSpec
		         name: 'Platform';
		         includes: #( 'Core' );
		         yourself.
	self assert: group name equals: 'Platform'.
	self assert: (group includes includes: 'Core').
	self should: [ group requires: #(  ) ] raise: Error.
	self assert: group isGroupSpec
]
