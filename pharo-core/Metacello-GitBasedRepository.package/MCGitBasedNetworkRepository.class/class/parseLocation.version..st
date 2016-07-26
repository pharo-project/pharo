private
parseLocation: locationUrl version: versionString
  "self 
	parseLocation: 'github://dalehenrich/MetacelloRepository:master/monticello/repos/itory/path'
	version: nil
     "

  | projectPath projectVersion repoPath headerSize desc projectDelim repoDelim versionDelim |
  headerSize := self description size.
  desc := locationUrl.
  desc := desc copyFrom: headerSize + 1 to: desc size.
  projectVersion := repoPath := nil.
  projectDelim := desc indexOf: $/.
  repoDelim := desc indexOf: $/ startingAt: projectDelim + 1.
  (versionDelim := desc indexOf: $:) == 0
    ifTrue: [ 
      repoDelim == 0
        ifTrue: [ projectPath := desc ]
        ifFalse: [ 
          projectPath := desc copyFrom: 1 to: repoDelim - 1.
          repoPath := desc copyFrom: repoDelim + 1 to: desc size ] ]
    ifFalse: [ 
      projectPath := desc copyFrom: 1 to: versionDelim - 1.
      repoDelim == 0
        ifTrue: [ projectVersion := desc copyFrom: versionDelim + 1 to: desc size ]
        ifFalse: [ 
          projectPath := desc copyFrom: 1 to: versionDelim - 1.
          self
            parseProjectVersionField: [ :pv :rp | 
              projectVersion := pv.
              repoPath := rp ]
            desc: desc
            versionDelim: versionDelim ] ].
  versionString ~~ nil
    ifTrue: [ projectVersion := versionString ].
  ^ self new
    projectPath: projectPath
    projectVersion: projectVersion
    repoPath: repoPath