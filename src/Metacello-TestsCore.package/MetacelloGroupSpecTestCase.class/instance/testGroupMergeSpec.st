tests
testGroupMergeSpec
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