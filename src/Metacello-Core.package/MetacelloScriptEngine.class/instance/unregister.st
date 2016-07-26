actions api
unregister
  | spec |
  self setDefaultsAndValidate: self projectSpec copy.	"don't add defaults"
  spec := self projectSpec.
  MetacelloProjectRegistration
    registrationForProjectSpec: spec
    ifAbsent: [ :ignored |  ]
    ifPresent: [ :existing :new | existing unregisterProject ].
  self root: spec