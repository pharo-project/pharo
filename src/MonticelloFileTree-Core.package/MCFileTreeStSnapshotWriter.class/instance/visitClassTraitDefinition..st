visiting
visitClassTraitDefinition: definition
    orderedTraitNames add: definition className, ' classTrait'.
    self classTraitDefinitions at: definition className put: definition