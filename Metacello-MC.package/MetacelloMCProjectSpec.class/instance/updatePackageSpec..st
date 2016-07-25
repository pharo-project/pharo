development support
updatePackageSpec: updatedSpecs
    "Add project copy to updatedSpecs if the current version of the project 
	 is different from the receiver's version"

    | prj currentVersion spec |
    className == nil
        ifTrue: [ ^ self ].
    prj := self projectClassProject.
    (currentVersion := prj currentVersion) = self versionOrNil
        ifTrue: [ ^ self ].
    currentVersion == nil
        ifTrue: [ ^ self ].
    spec := self copy.
    spec versionString: currentVersion versionString.
    updatedSpecs at: spec name put: spec