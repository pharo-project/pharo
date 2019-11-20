Class {
	#name : #MetacelloGroupSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsCore-Specs'
}

{ #category : #tests }
MetacelloGroupSpecTestCase >> testGroupMergeSpec [
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

{ #category : #tests }
MetacelloGroupSpecTestCase >> testGroupSpec [
	| group |
	group := self groupSpec
		name: 'Platform';
		includes: 'Core';
		yourself.
	self assert: group name equals: 'Platform'.
	self assert: (group includes includes: 'Core').
	group := self groupSpec
		name: 'Platform';
		includes: #('Core');
		yourself.
	self assert: group name equals: 'Platform'.
	self assert: (group includes includes: 'Core').
	self should: [ group requires: #() ] raise: Error.
	self should: [ group answers: #() ] raise: Error.
	group projectDo: [ :ignored | self assert: false ] packageDo: [ :ignored | self assert: false ] groupDo: [ :grp | self assert: group identicalTo: grp ]
]
