actions api
register
  | spec |
  self setDefaultsAndValidate: self projectSpec copy.	"don't add defaults"
  spec := self projectSpec.
  MetacelloProjectRegistration
    registrationForProjectSpec: spec
    ifAbsent: [ :new | new registerProject ]
    ifPresent: [ :existing :new | existing copyOnWrite: [ :existingCopy | existingCopy merge: new ] ].
  self root: spec