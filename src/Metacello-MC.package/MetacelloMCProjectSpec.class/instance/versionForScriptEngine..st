scripting
versionForScriptEngine: aMetacelloScriptEngine
    | prj |
    prj := self projectClass ifNil: [ self project ] ifNotNil: [ self projectClassProject ].
    ^ ((prj projectForScriptEngine: aMetacelloScriptEngine) version: self versionString)
        silently: aMetacelloScriptEngine silently;
        ignoreImage: aMetacelloScriptEngine ignoreImage;
        cacheRepository: aMetacelloScriptEngine cacheRepository;
        repositoryOverrides: aMetacelloScriptEngine repositoryOverrides