private
integerFromString: aString
  aString
    detect: [ :char | char isDigit not ]
    ifNone: [ 
      | integer |
      integer := aString asInteger.
      ((aString at: 1) = $0 and: [ aString size > 1 ])
        ifTrue: [ 
          self
            error:
              'invalid version number: normal version component must not have leading 0s'
                , aString asString ].
      ^ integer ].
  self
    error:
      'invalid version number: normal version component must be integer '
        , aString asString