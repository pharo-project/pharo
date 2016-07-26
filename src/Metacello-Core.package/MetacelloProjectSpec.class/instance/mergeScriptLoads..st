scripting
mergeScriptLoads: aSpec
    self shouldBeMutable.
    aSpec loads
        ifNotNil: [ :otherLoads | self loads ifNil: [ loads := otherLoads ] ifNotNil: [ loads := (loads , otherLoads) asSet asArray ] ].
    self loader: aSpec loader