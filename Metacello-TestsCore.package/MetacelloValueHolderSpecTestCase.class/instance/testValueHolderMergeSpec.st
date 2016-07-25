tests
testValueHolderMergeSpec
    | valueHolderA valueHolderB valueHolder |
    valueHolderA := self valueHolderSpec
        value: 'an Object';
        yourself.
    valueHolderB := self valueHolderSpec
        value: 1.1;
        yourself.
    valueHolder := valueHolderA mergeSpec: valueHolderB.
    self assert: valueHolder value = 1.1