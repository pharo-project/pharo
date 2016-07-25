accessing
packages
    packageList == nil
        ifTrue: [ packageList := self project packagesSpec ].
    ^ packageList