actions api
fetchRecord: fetchRecordBlock required: required
  MetacelloProjectRegistration
    copyRegistryWhile: [ 
      self
        handleNotificationsForAction: [ 
          | version loadedSpec |
          self setDefaultsAndValidateProjectSpec.
          [ loadedSpec := self lookupProjectSpecFor: self projectSpec ]
            on: MetacelloAllowProjectDowngrade , MetacelloAllowProjectUpgrade
            do: [ :notification | 
              notification
                handleOnDownGrade: [ :ex :existing :new | ex allowEvenIfLocked ]
                onUpgrade: [ :ex :existing :new | ex allowEvenIfLocked ] ].
          version := loadedSpec versionForScriptEngine: self.
          self root: (fetchRecordBlock value: version) loadDirective ] ]