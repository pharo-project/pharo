accessing
projectVersionPattern
  "do not set projectVersionPattern unless it _is_ a pattern: includes $#, %*, or $?"

  projectVersionPattern ifNil: [ ^ self projectVersion ].
  ^ projectVersionPattern