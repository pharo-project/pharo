tests
testValueHolderSpec
    | valueHolder |
    valueHolder := self valueHolderSpec
        value: 'an Object';
        yourself.
    self assert: valueHolder value = 'an Object'