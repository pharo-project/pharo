api-configuration
commitBaselineOfMethod
  | skipValidation isues |
  skipValidation := false.
  self methodSpec ifNil: [ ^ self ].
  self methodSpec compileMethod.
  project := project configuration class project.	"recalculate project"
  [ project version: self methodSpec versionString ]
    on: MetacelloSymbolicVersionDoesNotExistError
    do: [ :ex | 
      "symbolic versions can be removed"
      skipValidation := true ].
  skipValidation
    ifFalse: [ self validateVersion: self methodSpec versionString ].
  methodSpec := nil