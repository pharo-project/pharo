accessing
specials
    ^ specials ifNil: [ specials := self initializeSpecials ]