scripting
canUpgradeTo: aMetacelloProjectSpec
    ^ (super canUpgradeTo: aMetacelloProjectSpec) and: [ self file = aMetacelloProjectSpec file ]