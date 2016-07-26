accessing
isEmpty
  ^ self configurationProjectSpecs isEmpty
    and: [ self baselineProjectSpecs isEmpty ]