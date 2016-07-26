private
ensureReceiver: anObject
  "Return true if my receiver hasn't gone away"
  anObject ifNil: [^ false].
  (anObject class isObsolete) ifTrue: [^ false].
  (anObject isBehavior and: [anObject isObsolete]) ifTrue: [^ false].
  ^ true.
