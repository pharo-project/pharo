utilities
validate: expectedConfigurationClassName expConfigVersion: expectedConfigurationVersion expConfigRepo: expectedConfigurationRepository expBaselineClassName: expectedBaselineClassName expBaselineVersion: expectedBaselineVersion expBaselineRepo: expectedBaselineRepository
  MetacelloProjectRegistration registry configurationRegistry
    at: expectedConfigurationClassName
    ifPresent: [ :existing | 
      | spec x |
      spec := existing configurationProjectSpec.
      self
        assert: (x := spec version versionString) = expectedConfigurationVersion.
      self
        assert:
          (spec repositoryDescriptions includes: expectedConfigurationRepository) ]
    ifAbsent: [ self assert: expectedConfigurationVersion == nil ].
  MetacelloProjectRegistration registry baselineRegistry
    at: expectedBaselineClassName
    ifPresent: [ :existing | 
      | spec |
      spec := existing baselineProjectSpec.
      self assert: spec versionString = expectedBaselineVersion.
      self
        assert:
          (spec repositoryDescriptions includes: expectedBaselineRepository) ]
    ifAbsent: [ self assert: expectedBaselineVersion == nil ]