accessing
version
    ^ (self configurationProjectSpecIfAbsent: [ ^ MetacelloMCBaselineProject singletonVersionName ]) versionString