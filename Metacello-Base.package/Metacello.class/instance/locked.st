api actions
locked
  "list of locked projects in registry"

  ^ self
    project: [ :projectSpec | projectSpec isLocked ];
    list