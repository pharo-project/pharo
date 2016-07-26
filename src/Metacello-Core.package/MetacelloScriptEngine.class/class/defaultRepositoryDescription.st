defaults
defaultRepositoryDescription
    DefaultRepositoryDescription
        ifNil: [ DefaultRepositoryDescription := MetacelloPlatform current defaultRepositoryDescription ].
    ^ DefaultRepositoryDescription