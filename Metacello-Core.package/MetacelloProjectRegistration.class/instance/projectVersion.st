accessing
projectVersion
  | pv |
  pv := self versionInfo projectVersion.
  pv
    ifNil: [ 
      pv := self projectSpec versionOrNil.
      self versionInfo projectVersion: pv ].
  ^ pv