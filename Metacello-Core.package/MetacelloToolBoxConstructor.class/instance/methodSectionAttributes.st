accessing
methodSectionAttributes
    | attributes |
    attributes := Set new.
    self methodSectionsDo: [ :methodSection | attributes addAll: methodSection attributes ].
    ^ attributes