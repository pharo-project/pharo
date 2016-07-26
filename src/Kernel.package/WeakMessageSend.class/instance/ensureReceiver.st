private
ensureReceiver
  "Return true if my receiver hasn't gone away"
  self receiver ifNil: [^ false].
  (self receiver class isObsolete) ifTrue: [^ false].
  (self receiver isBehavior and: [self receiver isObsolete]) ifTrue: [^ false].
  ^ true.
