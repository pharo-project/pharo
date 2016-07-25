utilities
validateProjects: specArrays
  specArrays
    do: [ :array | 
      self
        validate: (array at: 1)
        expConfigVersion: (array at: 2)
        expConfigRepo: (array at: 3)
        expBaselineClassName: (array at: 4)
        expBaselineVersion: (array at: 5)
        expBaselineRepo: (array at: 6) ]