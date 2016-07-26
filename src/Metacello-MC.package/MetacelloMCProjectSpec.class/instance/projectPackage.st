accessing
projectPackage
    projectPackage
        ifNil: [ 
            self className ifNil: [ ^ nil ].
            projectPackage := self project packageSpec.
            projectPackage name: self className.
            self getFile ifNotNil: [ projectPackage file: self file ].
            projectPackage repositories: self getRepositories ].
    ^ projectPackage