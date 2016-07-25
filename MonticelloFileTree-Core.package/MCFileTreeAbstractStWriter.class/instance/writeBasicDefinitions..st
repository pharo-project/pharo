initialize-release
writeBasicDefinitions: aCollection
    "the correct initialization order is unknown if some classes are missing in the image"

    self writePropertiesFile.
    stWriter acceptVisitor: self forDefinitions: aCollection.
    self writeInitializers