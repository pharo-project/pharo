accessing
project
    ^ project
        ifNil: [ 
            "Bootstrap Metacello if it is not already loaded"
            self class ensureMetacello.
            project := self projectClass new projectAttributes: self customProjectAttributes.	"Create the Metacello project"
            project versionNumberClass: self versionNumberClass.
            project class versionConstructorClass on: self project: project.	"Construct the project"
            project loadType: #'linear'.	"change to #atomic if desired"
            project ]