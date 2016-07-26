actions api
unlock
  | spec |
  MetacelloProjectRegistration
    copyRegistryRestoreOnErrorWhile: [ 
      self setDefaultsAndValidate: self projectSpec copy.	"don't add defaults"
      spec := self projectSpec.
      MetacelloProjectRegistration
        registrationForProjectSpec: spec
        ifAbsent: [ :ignored |  ]
        ifPresent: [ :existing :new | existing copyOnWrite: [ :existingCopy | existingCopy locked: false ] ].
      self root: spec ]