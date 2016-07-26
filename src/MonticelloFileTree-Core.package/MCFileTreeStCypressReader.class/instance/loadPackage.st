accessing
loadPackage
    self hasMonticelloMetadata
        ifTrue: [ ^ super loadPackage ].
    package := MCPackage named: self packageNameFromPackageDirectory