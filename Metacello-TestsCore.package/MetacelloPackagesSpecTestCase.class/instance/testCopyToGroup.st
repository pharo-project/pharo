tests
testCopyToGroup
    "not currently used by MetacelloAbstractVersionConstructor"

    | packages group |
    packages := self packagesSpec.
    packages
        add:
            (self groupSpec
                name: 'Platform';
                includes: 'Core';
                yourself).
    packages
        merge:
            (self groupSpec
                name: 'Platform';
                includes: 'Tests';
                yourself).
    group := packages packageNamed: 'Platform' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Tests').
    group := self groupSpec
        name: 'PlatformCopy';
        includes: 'Copy';
        yourself.
    packages copy: 'Platform' to: group.
    group := packages packageNamed: 'PlatformCopy' ifAbsent: [ self assert: false ].
    self assert: (group includes includes: 'Core').
    self assert: (group includes includes: 'Copy').
    self assert: (group includes includes: 'Tests')