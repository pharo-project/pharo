accessing
repositoryDescriptions
    ^ (self configurationProjectSpecIfAbsent: [ self baselineProjectSpec ]) repositoryDescriptions