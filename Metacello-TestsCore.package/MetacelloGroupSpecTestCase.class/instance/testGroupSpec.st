tests
testGroupSpec
    | group |
    group := self groupSpec
        name: 'Platform';
        includes: 'Core';
        yourself.
    self assert: group name = 'Platform'.
    self assert: (group includes includes: 'Core').
    group := self groupSpec
        name: 'Platform';
        includes: #('Core');
        yourself.
    self assert: group name = 'Platform'.
    self assert: (group includes includes: 'Core').
    self should: [ group requires: #() ] raise: Error.
    self should: [ group answers: #() ] raise: Error.
    group
        projectDo: [ :ignored | self assert: false ]
        packageDo: [ :ignored | self assert: false ]
        groupDo: [ :grp | self assert: group == grp ]