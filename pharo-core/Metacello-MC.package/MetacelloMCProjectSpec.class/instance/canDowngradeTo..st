scripting
canDowngradeTo: aMetacelloProjectSpec
    ^ (super canDowngradeTo: aMetacelloProjectSpec) and: [ self file = aMetacelloProjectSpec file ]