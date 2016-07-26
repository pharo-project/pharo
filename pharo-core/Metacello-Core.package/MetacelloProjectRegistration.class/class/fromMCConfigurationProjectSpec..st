instance creation
fromMCConfigurationProjectSpec: aProjectSpec
    ^ self new
        projectName: aProjectSpec name;
        configurationProjectSpec: aProjectSpec;
        yourself