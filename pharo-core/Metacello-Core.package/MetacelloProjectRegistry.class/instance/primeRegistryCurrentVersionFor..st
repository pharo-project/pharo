initialization
primeRegistryCurrentVersionFor: cl
  ^ [ cl project currentVersion ]
    on: Error
    do: [ :ex | 
      Warning
        signal:
          'Error finding current version of ' , cl name asString
            , '. Probably an invalid specification.'.
      nil ]