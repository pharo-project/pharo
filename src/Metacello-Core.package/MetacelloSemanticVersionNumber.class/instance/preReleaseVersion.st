accessing
preReleaseVersion
    preReleaseVersion ifNil: [ preReleaseVersion := #() ].
    ^ preReleaseVersion